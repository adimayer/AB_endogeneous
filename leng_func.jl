# functions that run simulations
# periods
# generation
# run


function q_period(state, decide_para, econ_set, sim_set, account_a)
    #println()
    #println("time p ", state["time_p"])
    w_cash = state["w_cash"]
    f_cash = state["f_cash"]
    post_price = state["f_prices"]
    productivity = state["productivity"]
    w_debt = state["w_debt"]
    f_debt = state["f_debt"]
    capital = state["capital"]
    wage_net = state["wage_net"]
    wage_net_old = copy(wage_net)
    n_workers = length(w_cash)
    n_firms = length(f_cash)

    # create shock variables & sub period dummys
    sub_dummy_w = zeros(n_workers,econ_set["sub_periods"])
    sub_dummy_f = zeros(n_firms,econ_set["sub_periods"])
    if econ_set["shock_period"] > 0   # runup to shock
        if (state["time_p"] < econ_set["shock_period"] - 2) || (state["time_p"] > econ_set["shock_period"] + econ_set["shock_duration"] + 1)
            productivity_shock = 1
            heli_drop = 0
            sub_dummy_w[:,1] = ones(n_workers)
            sub_dummy_f[:,1] = ones(n_firms)
        elseif (state["time_p"] > econ_set["shock_period"] - 3) &&  (state["time_p"] < econ_set["shock_period"]) # pre shock
            productivity_shock = 1
            heli_drop = 0
            sub_dummy_w[:,2] = ones(n_workers)
            sub_dummy_f[:,2] = ones(n_firms)
        elseif (state["time_p"] > econ_set["shock_period"] - 1) &&  (state["time_p"] < econ_set["shock_period"] + econ_set["shock_duration"])  # during shock
            productivity_shock = econ_set["p_shock"]
            heli_drop = econ_set["m_shock"] - 1
            sub_dummy_w[:,3] = ones(n_workers)
            sub_dummy_f[:,3] = ones(n_firms)
        elseif (state["time_p"] > econ_set["shock_period"] + econ_set["shock_duration"] - 1)  &&  (state["time_p"] < econ_set["shock_period"] + econ_set["shock_duration"] + 2) # after shock
            productivity_shock = 1
            heli_drop = 0
            sub_dummy_w[:,4] = ones(n_workers)
            sub_dummy_f[:,4] = ones(n_firms)
        end
    else
        sub_period = Int(ceil(state["time_p"] / (sim_set["periods"]/econ_set["sub_periods"]) ))
        productivity_shock = econ_set["p_shock"][sub_period]
        heli_drop = econ_set["m_shock"][sub_period]-1
        sub_dummy_w[:,sub_period] = ones(n_workers)
        sub_dummy_f[:,sub_period] = ones(n_firms)
    end


    #0 Drop money
    # lump sum -- ratio
    #agent_drop = (sum(w_cash) + sum(f_cash)) * heli_drop / (n_workers + n_firms)
    #w_cash = maximum([(w_cash *.5) (w_cash + ones(n_workers) * agent_drop)], dims=2)
    #f_cash = maximum([(f_cash *.5) (f_cash + ones(n_firms) * agent_drop)], dims=2)

    #vLump sum


    w_cash = maximum([(w_cash *.5) (w_cash + ones(n_workers) * heli_drop)], dims=2)
    f_cash = maximum([(f_cash *.5) (f_cash + ones(n_firms) * heli_drop)], dims=2)


    # proportion
    #w_cash = w_cash * (1 + heli_drop )
    #f_cash = f_cash * (1 + heli_drop )
    # 1 Draw Productivity
    productivity = q_draw_productivity_g(productivity, econ_set["productivity_transition"])
    real_productivity = (productivity  + ones(n_firms)) * productivity_shock # -  ones(n_firms)  #  if productivity shock continoius and want mean productivity = 1

    # 2 Job search
    wage_net = q_job_search(wage_net, post_price, real_productivity, capital, econ_set)

    # 3	Produce
    n_employee = sum((wage_net.> 0), dims = 2)
    jobq = sum((wage_net.> 0), dims = 1)
    u_rate = sum(n_employee)/sum(n_workers)
    output = broadcast(q_produce,n_employee, real_productivity, capital, econ_set["alpha"], econ_set["gamma"])

    # 4 decide worker rule
    w_wage = sum(wage_net, dims = 1)
    input = hcat(w_cash, w_wage', w_cash, sub_dummy_w)
    worker_save_rate = q_choice_wc(input, decide_para["w_consume"], sim_set["scale"])
    o_w_cash = copy(w_cash)
    w_shop_cash = w_cash .* (ones(n_workers) - worker_save_rate)
    w_cash = w_cash - w_shop_cash


    # 5 decide investment demand firms
    input = hcat(f_cash, capital, real_productivity, sub_dummy_f )
    firm_invest_rate = q_choice_fi(input, decide_para["f_invest"], sim_set["scale"])  # share of cash for new capital
    f_invest_cash = f_cash .* firm_invest_rate
    f_cash = f_cash - f_invest_cash

    # 6 decide cash savings firms
    input = hcat(f_cash, capital, real_productivity, sub_dummy_f)
    firm_consume_rate = q_choice_fc(input, decide_para["f_consume"], sim_set["scale"])  # share of remaing cash for new capital
    f_shop_cash = f_cash .* (ones(n_firms) - firm_consume_rate)
    f_cash = f_cash - f_shop_cash


    # 7 Shop - sellers: firms buyers: workers, firm consumer, firm capital
    #input = hcat(f_cash, capital, real_productivity,  f_shock_before, f_shock_during, f_shock_after)
    #price_expect = q_choice_fc(input, decide_para["f_price"], sim_set["scale"])

    d_cash = vcat(w_shop_cash,f_invest_cash,f_shop_cash)   # compbine demand from workers and firms
    bought, revenue, m_price, l_cash, supply_left, post_price = q_market_gen(output, d_cash, post_price, econ_set)
    if sim_set["market_type"] == 0
        post_price = ones(n_firms) * mean(m_price)
    end
    #println("supply_left ", supply_left ./ output )
    #println("post_price ", post_price)
    share_sold = sum(bought) / sum(output)
    w_cash = w_cash + l_cash[1:n_workers]
    f_cash = f_cash + l_cash[n_workers+1:n_workers+n_firms] + l_cash[n_workers+1+n_firms:end] + revenue
    w_consume = bought[1:n_workers]
    f_invest = bought[n_workers+1:n_workers+n_firms]
    f_consume = bought[n_workers+1+n_firms:end]


    # 8  Resulting  allocation
    # Utilities
    w_utility = q_utility_gen(w_consume, econ_set["sigma"])
    f_utility = q_utility_gen(f_consume, econ_set["sigma"])
    utility = vcat(w_utility,f_utility)
    # Capital next period
    capital = econ_set["delta"] * capital + f_invest


    # 9 Pay worker  --- inludes firing workers if cannot pay
    wage_net, f_cash, w_cash = q_pay(wage_net, f_cash, w_cash)
    s_hired = sum(((wage_net.> 0) - (wage_net_old.> 0).>0)) / n_workers

    # update state at end of period
    state["w_cash"] = w_cash
    state["f_cash"] = f_cash
    state["f_prices"] = post_price
    state["productivity"] = productivity
    state["w_debt"] = w_debt
    state["f_debt"] = f_debt
    state["capital"] = capital
    state["wage_net"] = wage_net

    p_account = Dict("worker_save_rate" => worker_save_rate,  "firm_consume_rate" => firm_consume_rate,
    "firm_invest_rate" => firm_invest_rate, "share_sold" => share_sold,  "firm_consume_level" => f_consume,
    "worker_consume_level" => w_consume, "firm_invest_level" => f_invest, "output" => output, "m_price" => m_price)

    append!(account_a["sum_output"] , sum(output))
    append!(account_a["mean_unemploy"] , u_rate)
    append!(account_a["mean_price"] , mean(m_price))
    append!(account_a["mean_save_w"] , mean(worker_save_rate))
    append!(account_a["mean_save_fi"] , mean(firm_invest_rate))
    append!(account_a["mean_save_fc"] , mean(firm_consume_rate))
    append!(account_a["mean_consume_w"] , mean(w_consume))
    append!(account_a["mean_consume_f"] , mean(f_consume))
    append!(account_a["mean_invest_f"] , mean(f_invest))
    append!(account_a["sum_capital"] , sum(capital))
    append!(account_a["mean_wage"] , mean(w_wage))
    append!(account_a["worker_move"] , s_hired)


    return state, utility, account_a, p_account
end

function q_generation(state, decide_para, econ_set, sim_set, account_a)
    periods = sim_set["periods"]
    beta = econ_set["beta"]
    wage_net = state["wage_net"]
    n_f, n_w = size(wage_net)
    gen_utility = zeros(n_w + n_f)
    p_account = Dict()
    state["time_p"] = 0
    for i in 1:periods
        state["time_p"] = state["time_p"] + 1
        state,  utility, account_a, p_account = q_period(state, decide_para, econ_set, sim_set, account_a)
        #println("g_utility ", utility)
        gen_utility = gen_utility * (1/beta) +  utility
        #println("gen_utility ", gen_utility)
    end
    return gen_utility, state, account_a, p_account
end

function q_run(state, decide_para, econ_set, sim_set, account_econ, account_para)
    account_b = deepcopy(account_a)
    wage_net = state["wage_net"]
    n_f, n_w = size(wage_net)
    generations = sim_set["generations"]
    p_account = Dict()
    p_mutate = sim_set["mutation_prob"]
    for d in 1:generations
        println("Generation: ", d)
        disinflate = econ_set["money_supply"] * ( n_f + n_w ) / (sum(state["w_cash"]) + sum(state["f_cash"]))
        state["w_cash"] = state["w_cash"] * disinflate
        state["f_cash"] = state["f_cash"] * disinflate
        if econ_set["reset_capital"] == 1    # reset capital to zero
            state["w_cash"] = ones(n_w) * mean(state["w_cash"])
            state["f_cash"] = ones(n_f) * mean(state["f_cash"])
            state["w_debt"] = ones(n_w) * mean(state["w_debt"])
            state["f_debt"] = ones(n_f) * mean(state["f_debt"])
            state["capital"] = ones(n_f) * mean(state["capital"])
        end
        if d==401
            sim_set["mutation_prob"] =.01
        end
        if d==601
            sim_set["mutation_prob"] =.01
        end
        if d==1901
            sim_set["mutation_prob"] =.002
        end


        gen_utility, state, account_a, p_account = q_generation(state, decide_para, econ_set, sim_set, account_econ)
        # learn
        decide_para["w_consume"] = q_learn(gen_utility[1:n_w], decide_para["w_consume"], sim_set["mutation_prob"])
        decide_para["f_invest"] = q_learn(gen_utility[n_w+1:end], decide_para["f_invest"], sim_set["mutation_prob"])
        decide_para["f_consume"] = q_learn(gen_utility[n_w+1:end], decide_para["f_consume"], sim_set["mutation_prob"])
        #m_para = hcat(mean(decide_para["w_consume"],dims=1), mean(decide_para["f_invest"],dims=1), mean(decide_para["f_consume"],dims=1))
        #account_para=vcat(account_para,m_para)

        account_para["para_w_consume"] = vcat(account_para["para_w_consume"],mean(decide_para["w_consume"],dims=1))
        account_para["para_f_invest"] = vcat(account_para["para_f_invest"],mean(decide_para["f_invest"],dims=1))
        account_para["para_f_consume"] = vcat(account_para["para_f_consume"],mean(decide_para["f_consume"],dims=1))

        append!(account_b["sum_output"] , account_a["sum_output"][end])
        append!(account_b["mean_unemploy"] , account_a["mean_unemploy"][end])
        append!(account_b["mean_price"] , account_a["mean_price"][end])
        append!(account_b["mean_save_w"] , account_a["mean_save_w"][end])
        append!(account_b["mean_save_fi"] , account_a["mean_save_fi"][end])
        append!(account_b["mean_save_fc"] , account_a["mean_save_fc"][end])
        append!(account_b["mean_consume_w"] , account_a["mean_consume_w"][end])
        append!(account_b["mean_consume_f"] , account_a["mean_consume_f"][end])
        append!(account_b["mean_invest_f"] , account_a["mean_invest_f"][end])
        append!(account_b["sum_capital"] , account_a["sum_capital"][end])
        append!(account_b["mean_wage"] , account_a["mean_wage"][end])
        append!(account_b["worker_move"] , account_a["worker_move"][end])

    end
    return account_b, account_para, decide_para, state, p_account
end

function q_sim_one(state, decide_para, econ_set, sim_set, account_econ)
    sum_output = zeros(0)
    mean_unemploy = zeros(0)
    mean_price = zeros(0)
    mean_save_w = zeros(0)
    mean_save_fi = zeros(0)
    mean_save_fc = zeros(0)
    mean_consume_w = zeros(0)
    mean_consume_f = zeros(0)
    mean_invest_f = zeros(0)
    sum_capital = zeros(0)
    mean_wage = zeros(0)
    worker_move = zeros(0)
    n_f, n_w = size(state["wage_net"])

    account_g = Dict("sum_output" => sum_output, "mean_unemploy" => mean_unemploy, "mean_invest_f" => mean_invest_f,  "worker_move" => worker_move,
    "mean_price" => mean_price, "mean_save_w" => mean_save_w, "mean_save_fi" => mean_save_fi, "mean_save_fc" => mean_save_fc,
    "mean_consume_w" => mean_consume_w, "mean_consume_f" => mean_consume_f , "sum_capital" =>  sum_capital, "mean_wage" => mean_wage  )

    disinflate = econ_set["money_supply"] * ( n_f + n_w ) / (sum(state["w_cash"]) + sum(state["f_cash"]))
    state["w_cash"] = state["w_cash"] * disinflate
    state["f_cash"] = state["f_cash"] * disinflate
    gen_utility, state, account_g, p_account = q_generation(state, decide_para, econ_set, sim_set, account_g)
    println("mean p ", mean(state["productivity"]))
    println("mean c ", mean(state["capital"]))
    return state, account_g, p_account
end
