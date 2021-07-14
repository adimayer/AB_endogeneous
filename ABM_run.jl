# legnick with capital

# choices: chash (1) hh_consume (2) firm_invest (3) firm_shop

ENV["GKS_ENCODING"] = "utf-8"
using LinearAlgebra, Statistics,  Distributions, Random,  Plots, JLD2

include("ABM_func.jl")
include("support_func.jl")
include("show_functions.jl")



function q_set_inital(rseed)
    # model type
    bond_market = 0  # with bond market
    choice_type = 5 # type of decsion function - 0 simple function /  1 linear / 2 neeural net / 3  shock_only / 4 shock_linear / 5 sub_periods
    market_type = 0 # 0 market clear; 1 artificial friction; 2 seller post buyer search
    prod_trans_type = 1 # 0 discrete ; 1 normal; 2 innovation
    production_type = 0 # cobb douglas

    # simulation parameter
    n_f = 50
    n_w = 100

    periods = 350 #100
    generations = 100 #250
    reset_capital = 1  # if =1 capital and cahs reset for each new generations

    # choice and learning parameters
    scale = 5  # inside decsion function - maps (0;1) parameters to (-scale;+scale) used inside sigmoid function
    mutation_prob = .01  # probablity that new learning parmeter is mutated

    sim_set = Dict("rseed" => rseed, "n_f" => n_f, "n_w" => n_w, "periods" => periods, "generations" => generations,
    "bond_market" => bond_market, "choice_type" => choice_type, "market_type" => market_type, "scale" => scale, "mutation_prob" => mutation_prob)

    # economy parameters
    product_trans_p = [0, 0]  # changed belwo when setting up function
    alpha = .25 # for production function coefficient for capital
    gamma = 0.5 # coefficent for labor
    beta = .975 # discount rate utility view like standart coeffiencent in intertemp model
    delta = 0.5 # capital depreciation
    sigma = 1 # coefficent for crra  utility functions >1, set to 1 (or lower) to get log utility

    money_supply = 100

    price_para = [5, .2, 0.1]  # changed when seting up market function

    # labor market
    search_friction = 0.1  # probability of not seeing job offer in period
    search_looks = 1 # of times getting to look at job opening ---- low frictions set this high and above to 0 -- otherwise this to 1
    wage_friction = 1 # switch job only if new offer exceeds old one times this ratio
    reservation_wage = 0

    sub_periods = 10
    shock_period = 275 # 0 equaliy sized  subperiods ___ 1
    shock_duration = 3
    if shock_period > 0
        sub_periods = 4
        p_shock = .5 # = 1 no shock
        m_shock = 1   # = 1 no drop
    else
        p_shock = ones(sub_periods)   # = 1 no shock
        #p_shock[4] = .5
        m_shock = ones(sub_periods)   # = 1 no drop
        #m_shock[5] = 2  # 18 -- 2  or 0
    end

    econ_set = Dict( "prod_trans_type"=> prod_trans_type, "productivity_transition"=> product_trans_p, "production_type" => production_type, "alpha" => alpha,
    "gamma" => gamma, "beta" => beta, "sigma" => sigma, "delta" => delta, "reset_capital" => reset_capital, "price_para" => price_para,
    "search_looks" => search_looks, "search_friction" => search_friction, "wage_friction" => wage_friction, "reservation_wage" => reservation_wage,
    "sub_periods" => sub_periods, "shock_period" => shock_period, "shock_duration" => shock_duration, "p_shock" => p_shock, "m_shock"  => m_shock, "money_supply" => money_supply  )

    if choice_type == 0   # set number of parameters for decsion function
        n_para_wc = 1
        n_para_fi = 1
        n_para_fc = 1
    elseif choice_type == 1
        n_para_wc = 3
        n_para_fi = 4
        n_para_fc = 4
    elseif choice_type == 2
        n_para_wc = 6
        n_para_fi = 12
        n_para_fc = 12
    elseif choice_type == 3   # set number of parameters for decsion function
        n_para_wc = 4
        n_para_fi = 4
        n_para_fc = 4
    elseif choice_type == 5   # set number of parameters for decsion function
        n_para_wc = sub_periods
        n_para_fi = sub_periods
        n_para_fc = sub_periods
    end

    # initial values
    productivity = ones(n_f)
    f_prices = rand(n_f)
    w_cash = ones(n_w) * money_supply
    f_cash = ones(n_f) * money_supply
    capital = ones(n_f) * 2.0
    w_debt =  ones(n_w) * 0.0
    f_debt =  ones(n_f) * 0.0
    wage_net = zeros(n_f, n_w)
    time_p = 0

    state = Dict("w_cash" => w_cash, "f_cash" => f_cash, "f_prices" => f_prices, "w_debt" => w_debt , "f_debt" => f_debt,
    "productivity" => productivity, "capital" => capital, "wage_net" => wage_net, "time_p" => time_p )

    pw_consume = q_draw_para(n_w, n_para_wc)
    pf_invest = q_draw_para(n_f, n_para_fi)
    pf_consume = q_draw_para(n_f, n_para_fc)
    decide_para = Dict( "w_consume" => pw_consume, "f_invest" => pf_invest, "f_consume" => pf_consume )


    # accounting
    # economy features recordecd every period
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
    account_a = Dict("sum_output" => sum_output, "mean_unemploy" => mean_unemploy,
    "mean_price" => mean_price, "mean_save_w" => mean_save_w, "mean_save_fi" => mean_save_fi, "mean_save_fc" => mean_save_fc, "mean_invest_f" => mean_invest_f,
    "mean_consume_w" => mean_consume_w, "mean_consume_f" => mean_consume_f , "sum_capital" =>  sum_capital, "mean_wage" => mean_wage, "worker_move" => worker_move  )

    # parameters for decsions recorded every generation
    para_w_consume = zeros(0,n_para_wc)
    para_f_invest = zeros(0,n_para_fi)
    para_f_consume = zeros(0,n_para_fc)
    account_para = Dict("para_w_consume" => para_w_consume, "para_f_invest" => para_f_invest,     "para_f_consume" => para_f_consume )

    return sim_set, econ_set, state, decide_para, account_a, account_para
end

println()
println(" ****************************  Setup  *************************************************** ")

rseed = 1234
Random.seed!(rseed)
sim_set, econ_set, state, decide_para, account_a, account_para = q_set_inital(rseed)

if econ_set["sigma"] > 1
    q_utility_gen(consume, sigma) = q_utility_crra(consume, sigma)
    println("CRRA utility")
else
    q_utility_gen(consume, sigma) = q_utility_log(consume, sigma)
    println("log utility")
end

if sim_set["market_type"] == 0
    q_market_gen(supply, demand, post_price, econ_set) = q_market_clear(supply, demand, post_price, econ_set)
    println("market_clears")
elseif sim_set["market_type"] == 1
    q_market_gen(supply, demand, post_price, econ_set) = q_market_friction(supply, demand, post_price, econ_set)
    econ_set["price_para"] = [0.8, 5, 0.1]  # [0.8, 5, 0.1]  dispersion, friction -- dispersion has to be between -1 and <1 ; -1 no dipersion  1 possible zero prices; friction has to be bigger than 0
elseif sim_set["market_type"] == 2
    q_market_gen(supply, demand, post_price, econ_set) = q_market_seller_post(supply, demand, post_price, econ_set)
    econ_set["price_para"] = [5, .25, 2]  # [5, .25, 4] number looks (>= 1), goal ratio (=>.2), adjustment speed (>0)
else
    println(" wrong " )
    stop
end

if econ_set["prod_trans_type"] == 0
    q_draw_productivity_g(productivity, trans_p) = q_draw_productivity_discrete(productivity, trans_p)
    econ_set["productivity_transition"] = [.1, .9]  # low to high, high to high
    println("Productivity low / high ", econ_set["productivity_transition"]  )
elseif econ_set["prod_trans_type"] == 1
    q_draw_productivity_g(productivity, trans_p) = q_draw_productivity_normal(productivity, trans_p)
    econ_set["productivity_transition"] = [.9, 0.1]  #  roh and sigma --  roh persitence -- sigma variance of new draw
elseif econ_set["prod_trans_type"] == 2
    q_draw_productivity_g(productivity, trans_p) = q_draw_productivity_inovate(productivity, trans_p)
    econ_set["productivity_transition"] = [.9, 0.1]  # decay number between 0 and 1 , prob innovation -- to double mean
else
    println(" wrong " )
    stop
end

if econ_set["production_type"]  == 0
    q_produce(n_employee, productvity, capital, alpha, gamma) = q_produce_cobb(n_employee, productvity, capital, alpha, gamma)
elseif econ_set["production_type"]  == 1
    q_produce(n_employee, productvity, capital, alpha, gamma) = q_produce_d(n_employee, productvity, capital, alpha, gamma)

end

if sim_set["choice_type"] == 0
    println("decsion rule one parameter")
    q_choice_wc(input, d_para, scale) = q_choice_simple(input, d_para, scale)
    q_choice_fi(input, d_para, scale) = q_choice_simple(input, d_para, scale)
    q_choice_fc(input, d_para, scale) = q_choice_simple(input, d_para, scale)
elseif sim_set["choice_type"] == 1
    println("decsion rule linear")
    q_choice_wc(input, d_para, scale) = q_choice_linear_two(input, d_para, scale)
    q_choice_fi(input, d_para, scale) = q_choice_linear_three(input, d_para, scale)
    q_choice_fc(input, d_para, scale) = q_choice_linear_three(input, d_para, scale)
elseif sim_set["choice_type"] == 2
    println("decsion rule neural network")
    q_choice_wc(input, d_para, scale) = q_choice_nn_two(input, d_para, scale)
    q_choice_fi(input, d_para, scale) = q_choice_nn_three(input, d_para, scale)
    q_choice_fc(input, d_para, scale) = q_choice_nn_three(input, d_para, scale)
elseif sim_set["choice_type"] == 3
    println("decsion rule neural network")
    q_choice_wc(input, d_para, scale) = q_choice_linear_shock(input, d_para, scale)
    q_choice_fi(input, d_para, scale) = q_choice_linear_shock(input, d_para, scale)
    q_choice_fc(input, d_para, scale) = q_choice_linear_shock(input, d_para, scale)
elseif sim_set["choice_type"] == 5
    println("sub_periods")
    q_choice_wc(input, d_para, scale) = q_choice_linear_shock(input, d_para, scale)
    q_choice_fi(input, d_para, scale) = q_choice_linear_shock(input, d_para, scale)
    q_choice_fc(input, d_para, scale) = q_choice_linear_shock(input, d_para, scale)
else
    println(" wrong " )
    stop
end

#state,  utility, account_a, p_account = q_period(state, decide_para, econ_set, sim_set, account_a)
#gen_utility, state, account_a, p_account = q_generation(state, decide_para, econ_set, sim_set, account_a)

println()
println(" ****************************  RUN  *************************************************** ")
println()

account_a, account_para, decide_para, state, p_account = q_run(state, decide_para, econ_set, sim_set, account_a, account_para)
println("simulation complete")

show_out = 2
if show_out == 1  # rep
    println("representative agent")
    show_set_para(sim_set, econ_set)  # not graph
    show_time(account_a)
    #println("simulate one gneration")
    #state, account_g, p_account  = q_sim_one(state, decide_para, econ_set, sim_set, account_a)
    #show_cross_rep(state,p_account)
    #show_time_generation_rep(account_g)
    #show_time_generation_rep_c(account_g,sim_set["n_f"])
elseif show_out == 2  # full
    show_set_para(sim_set, econ_set)
    #show_time(account_a)
    show_para(account_para)
    println("simulate one gneration")
    state, account_g, p_account  = q_sim_one(state, decide_para, econ_set, sim_set, account_a)
    #show_cross_full(state,p_account)
    show_time_generation_full(account_g)
    #numbers_full(account_g)

    # fake shock
    econ_set["p_shock"] = 1 # = 1 no shock
    econ_set["m_shock"] = 1   # = 1 no drop
    state, account_g, p_account  = q_sim_one(state, decide_para, econ_set, sim_set, account_a)
    #show_cross_full(state,p_account)
    show_time_generation_full(account_g)
    #numbers_full(account_g)


elseif show_out == 3  # rep fake shock
    println("representative agent  sim fake shock")
    show_set_para(sim_set, econ_set)  # not graph
    println("simulate one gneration")
    state, account_g, p_account  = q_sim_one(state, decide_para, econ_set, sim_set, account_a)
    show_cross_rep(state,p_account)
    show_time_generation_rep_b(account_g,sim_set["n_f"])
    println("simulate extra gneration no shock")
    econ_set["p_shock"] = 1 # = 1 no shock
    econ_set["m_shock"] = 1   # = 1 no drop
    #econ_set["p_shock"]= ones(econ_set["sub_periods"])
    state, account_g, p_account  = q_sim_one(state, decide_para, econ_set, sim_set, account_a)
    #show_cross_rep(state,p_account)
    show_time_generation_rep_b(account_g,sim_set["n_f"])
    numbers_full(account_g)
else
    println(" change to show_out in len_run to get graphs")
    #show_decsion(decide_para, sim_set)

end

#input = hcat(w_cash, w_wage')
#worker_save_rate = q_choice_wc(input, decide_para["w_consume"], sim_set["scale"])
