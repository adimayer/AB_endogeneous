#=
1. draw productivity ; wage offer ; job search ; produce ; pay
2. choice  support functions; learning; choice functions
3. utility functions
4. Markets
=#

function q_draw_productivity_discrete(a_productivity, product_trans)
    # give current productivity, transition probabilities [from low to high and high to high]
    # return new productivity_transition
    # discrete productivity eithe one or two
    productivity = a_productivity / 2
    n = length(productivity)
    productivity = (ones(n) - productivity) .* ((rand(n) .< product_trans[1]) .* ones(n)) + productivity .* ((rand(n) .< product_trans[2]) .* ones(n))
    a_productivity = productivity * 2
    return a_productivity
end

function q_draw_productivity_normal(productivity, product_trans)
    # give current productivity, transition paramaters [roh - sigma], type
    # return new productivity
    # roh -- gives persistence -- sigma variance of draw from normal distribution
    # lower bound zero -- for large variance skewed distribution
    n = length(productivity)
    nnn = Normal(0, product_trans[2])
    qq = product_trans[1] * log.(productivity) + rand(nnn,n)
    productivity = exp.(qq)
    return productivity
end

function q_draw_productivity_inovate(productivity, product_trans)
    # give current productivity, transition probabilities [ decay new inovation]
    # return new productivity_transition
    # product_trans[1] -- decay number between 0 and 1 -- if bigger one continious improvement
    # product_trans[2] -- prob innovation -- to double mean
    n = length(productivity)
    productivity = productivity * product_trans[1]
    productivity = productivity + ones(n) .* (rand(n) .< product_trans[2])
    #productivity = productivity + mean(productivity) * (rand(n) .< product_trans[2])
    return productivity
end


function q_wage_offer_scalar(n_employee, price, productvity, capital, alpha, gamma)
    # on scalar for broadcasting
    nn_employee = n_employee + 1
    output = q_produce(n_employee, productvity, capital, alpha, gamma)
    output_p = q_produce(nn_employee, productvity, capital, alpha, gamma)
    mvp = price * (output_p - output)
    wage = mvp * (rand(1) + [2.0]) / 3.0
    #println("mvp ", mvp)
    return wage[1]
end

function q_job_search(wage_net, prices, productivity, capital, econ_set)
    search_friction = econ_set["search_friction"]   # probability of not seeing offer in given period
    search_looks = econ_set["search_looks"]   # probability of not seeing offer in given period
    wage_friction = econ_set["wage_friction"]  # switch job only if new offer exceeds old one by this ratio
    reservation_wage = econ_set["reservation_wage"]  # switch job only if new offer exceeds old one by this ratio
    n_f, n_w = size(wage_net)
    w_wage = sum(wage_net, dims = 1)
        # loop randomly over all workers
    for i in randperm(n_w)
        temp_r = rand()
        l = 0
        while l < search_looks
            if temp_r > search_friction
                see = rand(1 : n_f)  # see one wage wage_offer
                i_employees = sum((wage_net[see,:].> 0))
                offer = q_wage_offer_scalar(i_employees, prices[see], productivity[see], capital[see], econ_set["alpha"], econ_set["gamma"])
                #println("w_wage[i] ",  w_wage[i], "    offer ", offer)
                if  offer > w_wage[i] * wage_friction   # if better offer switch employer
                    if offer > reservation_wage
                        wage_net[:,i] = zeros(n_f)      #  leave old employer
                        wage_net[see,i] = offer           #  new employment relationship
                        #println("new wage ", wage_net[see,i])
                    end
                end
            end
            l = l + 1
        end
    end
    return wage_net
end

function q_job_search_old(wage_net, prices, productivity, capital, econ_set)
    search_friction = econ_set["search_friction"]   # probability of not seeing offer in given period
    wage_friction = econ_set["wage_friction"]  # switch job only if new offer exceeds old one by this ratio
    reservation_wage = econ_set["reservation_wage"]  # switch job only if new offer exceeds old one by this ratio
    n_f, n_w = size(wage_net)
    w_wage = sum(wage_net, dims = 1)
        # loop randomly over all workers
    for i in randperm(n_w)
        temp_r = rand()
        if temp_r > search_friction
            see = rand(1 : n_f)  # see one wage wage_offer
            i_employees = sum((wage_net[see,:].> 0))
            offer = q_wage_offer_scalar(i_employees, prices[see], productivity[see], capital[see], econ_set)
            if  offer > w_wage[i] * wage_friction   # if better offer switch employer
                if offer > reservation_wage
                    wage_net[:,i] = zeros(n_f)      #  leave old employer
                    wage_net[see,i] = offer           #  new employment relationship
                end
            end
        end
    end
    return wage_net
end

function q_produce_cobb(n_employee, productvity, capital, alpha, gamma)
    # is scalar
    output = .001 + productvity * (capital^alpha) * ((1+n_employee)^gamma)
    #output = 1 + (productvity + 1) * (capital^alpha)
    return output
end

function q_produce_d(n_employee, productvity, capital, alpha, gamma)
    # is scalar
    output = .001 + productvity * ((1+n_employee)^gamma)
    #output = 1 + (productvity + 1) * (capital^alpha)
    return output
end


function q_pay(wage_net, f_cash, hh_cash)
    n_f, n_w = size(wage_net)
    payroll = sum(wage_net, dims = 2)
    pay_r = f_cash - payroll
    fire = 0
    for i in 1 : n_f  # loop through all firms
        while pay_r[i] < 0  # if insolvent
            #println(" cannot pay ")
            fire = fire + 1
            qq = findall(x->x>0, wage_net[i,:])
            temp = sample(qq)
            wage_net[i,temp] = 0 # fire random
            pay_r[i] = f_cash[i] - sum(wage_net[i,:])

        end
    end
    f_cash = f_cash - sum(wage_net, dims = 2)
    hh_cash[1:n_w] = hh_cash[1:n_w] + sum(wage_net, dims = 1)'
    #if fire > 0
    #    println("fire ", fire)
    #end
    return wage_net, f_cash, hh_cash
end


##############################################

sigmoid(z::Real) = one(z) / (one(z) + exp(-z))

function q_nnet_two(input, f_FL, f_SL )
    # two layer neural network
    # give input and coefficients for first and second layer
    # coefficient values from -10 to +10
    # return output

    n , i_s = size(input)
    n , h_l, o_l = size(f_SL)
    output = rand(n, o_l)
    for i in 1:n
        HL = input[i,:]' * f_FL[i,:,:]
        #HL = broadcast(sigmoid,HL)
        output[i,:] = HL * f_SL[i,:,:]
    end
    output = broadcast(sigmoid,output)
    return output
end

function q_learn(utility, param, p_mutate)
    # give utility and parameters for each person
    # return new list of parameters
    # distribution of mutation draw -- can affect results
    # p_mutate probablity that each gen of offspring will mutate
    #println("gen_utility lll" , utility)
    utility = utility * (-1)   # maximize
    # creat matrix that can be sorted
    n , n_param  = size(param)
    id = [1:n;]
    A= [id utility param]
    A = A[sortperm(A[:, 2]), :]   # sort

    # select and breed
    n_breed = Int(floor(n/2))           # share of agents who get to breed
    breeders = A[1:n_breed,3:end]     # pick best performing agents
    bmix = breeders[shuffle(1:end),:]  # shuffle them
    lottery = rand(0:1,n_breed,n_param) # 0 or 1 random for number breeders x parameters
    replace = breeders .* lottery + bmix .* (ones(n_breed,n_param)-lottery) # newly created genes  combine genes of breeders

    # mutation -- mutate some of the new genes
    d = Binomial(1, p_mutate)
    mutate = rand(d,n_breed,n_param)
    mutation = q_draw_para(n_breed, n_param)  # draw new parameters  -- use for mutations
    replace = replace .* (ones(n_breed,n_param)-mutate)  + mutation .* mutate

    # replace
    A[end-n_breed+1:end,3:end] = replace
    A = A[sortperm(A[:, 1]), :]
    new_para = A[:,3:end]
    return new_para
end

function q_draw_para(n, n_para)
    # random draw parameters -- for n agents -- n_para each
    draws = rand(n, n_para)   # uniform distribution
    #draws = rand(n,n_param) .* rand(n,n_param) # skewed towards zero
    draws = round.(draws, digits = 2)
    return draws
end


##############

function q_choice_simple(input, d_para, scale)
    n, m = size(input)
    ss = scale * (2 * d_para[:,1] - ones(n))
    sss = ones(n)*.01 + broadcast(sigmoid,ss) *.98
    return sss
end

function q_choice_linear_one(input, para, scale)
    cash = input[:,1]
    capital = input[:,2]
    productivity = input[:,3]
    n = length(cash)
    temp_x = scale * ((2 * para[:,1] - ones(n)) + (productivity .* (2 * para[:,2] - ones(n)))  )
    sss = ones(n)*.01 + broadcast(sigmoid,temp_x) *.98
    return sss
end

function q_choice_linear_two(input, para, scale)
    cash = input[:,1]
    wage = input[:,2]
    n = length(cash)
    temp_x = scale * ((2 * para[:,1] - ones(n)) + (cash .* (2 * para[:,2] - ones(n))) + (wage .* (2 * para[:,3] - ones(n))))
    sss = ones(n)*.01 + broadcast(sigmoid,temp_x) *.98
    return sss
end

function q_choice_linear_three(input, para, scale)
    cash = input[:,1]
    capital = input[:,2]
    productivity = input[:,3]
    n = length(cash)
    temp_x = scale * ((2 * para[:,1] - ones(n)) + (cash .* (2 * para[:,2] - ones(n))) + (productivity .* (2 * para[:,3] - ones(n))) + (capital .* (2 * para[:,4] - ones(n)))  )
    sss = ones(n)*.01 + broadcast(sigmoid,temp_x) *.98
    return sss
end

function q_choice_nn_two(input, para, scale) # neural net inputs
    # give: capital (cash) and productivity, 6 parameters
    # return savings rate
    # neural network
    # calls q_nnet_two
    # scales coefficients first
    n_agents = length(input[:,1])
    para_u = scale * (2 * para - ones(n_agents,6))  # transform para from (0;1) to (-scale; + scale)
    FL = zeros(n_agents,2,2)
    FL[:,1,:] = para_u[:,1:2]
    FL[:,2,:] = para_u[:,3:4]
    SL = zeros(n_agents,2,1)
    SL[:,:,1] = para_u[:,5:6]
    inputs = input[:,1:2]

    qqq = ones(n_agents)*.01 + q_nnet_two(inputs, FL, SL ) *.98
    return qqq
end

function q_choice_nn_three(input, para, scale) # neural net inputs
    # give: capital (cash) and productivity, 6 parameters
    # return savings rate
    # neural network
    # calls q_nnet_two
    # scales coefficients first
    n_agents = length(input[:,1])
    para_u = scale * (2 * para - ones(n_agents,12))  # transform para from (0;1) to (-scale; + scale)
    FL = zeros(n_agents,3,3)
    FL[:,1,:] = para_u[:,1:3]
    FL[:,2,:] = para_u[:,4:6]
    FL[:,3,:] = para_u[:,7:9]
    SL = zeros(n_agents,3,1)
    SL[:,:,1] = para_u[:,10:12]
    inputs = input[:,1:3]

    qqq = ones(n_agents)*.01 + q_nnet_two(inputs, FL, SL ) *.98
    return qqq
end

function q_choice_linear_shock(input, para, scale)

    na, nb = size(input)

    sub_dummy = input[:,4:end]

    n, n_sub = size(sub_dummy)


    temp_x = scale * sum( ( sub_dummy .* (2 * para - ones(n,n_sub) )), dims = 2)
    sss = ones(n)*.01 + broadcast(sigmoid,temp_x) *.98
    return sss
end

function q_choice_sub_period(input, para, scale)
    s_before = input[:,4]
    s_during = input[:,5]
    s_after = input[:,6]
    n = length(s_after)
    temp_x = scale * ((2 * para[:,1] - ones(n)) + (s_before .* (2 * para[:,2] - ones(n))) + (s_during .* (2 * para[:,3] - ones(n))) + (s_after .* (2 * para[:,4] - ones(n)))  )
    sss = ones(n)*.01 + broadcast(sigmoid,temp_x) *.98
    return sss
end

function q_choice_linear_two_shock(input, para, scale)
    cash = input[:,1]
    wage = input[:,2]
    s_before = input[:,4]
    s_during = input[:,5]
    s_after = input[:,6]
    n = length(cash)
    temp_x = scale * ((2 * para[:,1] - ones(n)) + (cash .* (2 * para[:,2] - ones(n))) + (wage .* (2 * para[:,3] - ones(n)))
     + (s_before .* (2 * para[:,4] - ones(n))) + (s_during .* (2 * para[:,5] - ones(n))) + (s_after .* (2 * para[:,6] - ones(n))))
    sss = ones(n)*.01 + broadcast(sigmoid,temp_x) *.98
    return sss
end

function q_choice_linear_three_shock(input, para, scale)
    cash = input[:,1]
    capital = input[:,2]
    productivity = input[:,3]
    s_before = input[:,4]
    s_during = input[:,5]
    s_after = input[:,6]
    n = length(cash)
    temp_x = scale * ((2 * para[:,1] - ones(n)) + (cash .* (2 * para[:,2] - ones(n))) + (productivity .* (2 * para[:,3] - ones(n)))
    + (capital .* (2 * para[:,4] - ones(n))) + (s_before .* (2 * para[:,5] - ones(n))) + (s_during .* (2 * para[:,6] - ones(n))) + (s_after .* (2 * para[:,7] - ones(n)))  )
    sss = ones(n)*.01 + broadcast(sigmoid,temp_x) *.98
    return sss
end

##############################


function q_utility_log(consume, sigma)
    # give consumption return utility
    n = length(consume)
    utility = log.(consume + ones(n)*.0001)
    return utility
end

function q_utility_crra(consume, sigma)
    # give consumption return utility
    #sigma = 1.5
    n = length(consume)
    consume = consume + ones(n)*.00001
    utility = (consume .^ (1-sigma )) ./ (1 - sigma )
    return utility
end


###########  Markets #################

function q_market_clear(supply, demand, post_price, econ_set)
    # demand is cash -- supply is in goods
    # market clears -- divide cash by supply to determine price
    n = length(demand)
    t_supply = sum(supply)
    t_cash = sum(demand)
    # check if supply and demand are both positive
    if (t_supply > 0) && (t_cash > 0 )
        price = t_cash / t_supply
        bought = demand / price
        revenue = supply * price
        supply_left = supply * 0
        cash_left = demand * 0
        price_paid = ones(n) * price
    else
        bought = demand * 0
        revenue = supply * 0
        price = 0
        println("no trade c")
        stop
        supply_left = supply
        cash_left = demand
        price_paid = ones(n) * price
    end

    return bought, revenue, price_paid, cash_left, supply_left, post_price
end

function q_market_friction(supply_o, demand, post_price, econ_set)
    supply = copy(supply_o)
    # market with price dispersion and friction
    # prices disperes around market clearing equilibrium
    # price para gives [1] dispersion and [2] friction
    dispersion = econ_set["price_para"][1] #  dispersion has to be between -1 and <1 ; -1 no dipersion  1 possible zero prices --
    friction  = econ_set["price_para"][2] #  friction has to be bigger than 0
    n_d = length(demand)
    n_s = length(supply)
    revenue = zeros(n_s)
    bought = zeros(n_d)
    price_sold = zeros(n_s)
    t_supply = sum(supply)
    t_demand = sum(demand)


    # check if supply and demand are both positive and calculate equilibrium price / quantity
    if (t_supply > 0) && (t_demand > 0 )
        eq_price = t_demand / t_supply
        eq_bought = demand / eq_price
        eq_revenue = supply * eq_price
    else
        bought = supply * 0
        revenue = supply * 0
        eq_price = 0
        println("no trade")
    end

    rand_d = randperm(n_d)  # shuffeled list of buyers
    dd = 1
    buyer = rand_d[dd]
    # loop through sellers in random order
    for seller in randperm(n_s)
        go_s = 0
        rrr= rand()
        price = eq_price * (rand() * (1 + dispersion ) + .5 * (1 - dispersion))
        price_sold[seller] = price
        #println("Price ", price)
        while supply[seller] > .0001 && go_s < 3
            buyer = rand_d[dd]
            q_demand = demand[buyer] / (price * (1 + rand() * friction))
            quant_d = min(supply[seller], q_demand)
            supply[seller] = supply[seller] - quant_d
            bought[buyer] = bought[buyer] + quant_d
            revenue[seller] = revenue[seller] + quant_d * price
            demand[buyer] = demand[buyer] - quant_d * price
            dd = dd + 1
            go_s = go_s + 1
            if dd > n_d
                dd = 1
            end
        end
    end

    return bought, revenue, price_sold, demand, supply, post_price
end

function q_market_seller_post(supply, d_cash, post_price, econ_set)
    # demand is cash -- supply is in goods
    # market may not clears
    # calls search post   and then adjusts prices
    price_para = econ_set["price_para"]
    n_look = Int(price_para[1])
    goal_ratio = price_para[2]
    adjust_speed = price_para[3]
    #println("post_price ", post_price)
    #println("supply ", supply)
    #println("d_cash ", d_cash)
    bought, revenue, price_paid, l_cash, supply_left  = q_post_search(supply, post_price, d_cash, n_look)
    new_price =q_price_adjust(post_price, supply, supply_left, goal_ratio, adjust_speed)
    return bought, revenue, price_paid, l_cash, supply_left, new_price
end

function q_post_search(init_supply, post_price, init_cash, n_look)
    # println("MARKET SEARCH")
    # give suppy in goods, demand in cash , inital posted price of sellers, number of sellers buyer looks at
    # return amount bought, revenue, price paid, cash left, supply left
    # sellers post price;
    # buyers in random order look at n_look sellers each
    # pick seller the would lead to biggest volume purchase -- depends on price and avaiable supply

    # define variables
    n_s = length(init_supply)
    n_d = length(init_cash)
    revenue = zeros(n_s)
    bought = zeros(n_d)
    price_paid = zeros(n_d)
    supply = copy(init_supply)
    d_cash = copy(init_cash)

    # loop in random order over all shoppers
    for i in randperm(n_d)
        offer_id=Int.(zeros(n_look))
        offer_amount = zeros(n_look)
        #look at n_look sellers
        for look in 1:n_look
            offer_id[look] = rand(1:n_s)
            if offer_id == i   # cannot buy from self
                offer_amount[look] = 0
            else
                offer_amount[look] = min(d_cash[i] / post_price[offer_id[look]], supply[offer_id[look]]) *.95
            end
        end
        # find best offer

        best_amount, best_look = findmax(offer_amount)
        best_id = offer_id[best_look]

        # transaction
        bought[i] = best_amount
        supply[best_id] = supply[best_id] - best_amount
        d_cash[i] = d_cash[i] - best_amount * post_price[best_id]
        revenue[best_id] = revenue[best_id] + best_amount * post_price[best_id]
        price_paid[i] = post_price[best_id]
        if minimum(d_cash)<0
            println("YYY")
            aa = findmin(d_cash)
            println(aa)
        end
    end
    return bought, revenue, price_paid, d_cash, supply
end

function q_price_adjust(old_price, inital_supply, left_supply, goal_ratio, adj_rate)
    # give old_price posted, amount supplied, amount left ofter selleing, goal of which share should be left, adjustment speed
    # goal ratio --(share sold)  if ratio smaller than that increase, if ratio bigger than para bigger decrease
    #  magintude of price change: zero no change, bigger bigger change
    ratio = left_supply ./ inital_supply
    n_price = old_price .* (exp.(goal_ratio .- ratio)) .^ adj_rate
    return n_price
end
