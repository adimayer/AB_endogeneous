# functions to display results



function show_set_para(sim_set, econ_set)
    println()
    println("Parameters for simulation")
    println(sim_set)
    println(econ_set)
end

function show_time(account_a)
    # time series macro data
    println("show time")
    waste_b = 1
    println(".")

    p1 = plot(account_a["sum_output"][waste_b:end] , title ="s_output",  label ="")
    p2 = plot(account_a["sum_capital"][waste_b:end] , title ="s_capital",  label ="")
    p3 = plot(account_a["mean_unemploy"][waste_b:end] , title ="m_employ",  label ="")
    p4 = plot(account_a["mean_price"][waste_b:end] , title ="m_price",  label ="")

    p5 = plot(account_a["mean_consume_w"][waste_b:end] , title ="m_consume_w",  label ="")
    p6 = plot(account_a["mean_consume_f"][waste_b:end] , title ="m_consume_f",  label ="")
    p7 = plot(account_a["mean_save_w"][waste_b:end] , title ="m_save_w",  label ="")
    p8 = plot(account_a["mean_save_fi"][waste_b:end] , title ="m_invest_f",  label ="")
    p9 = plot(account_a["mean_save_fc"][waste_b:end] , title ="m_save_f",  label ="")
    p10 = plot(account_a["mean_wage"][waste_b:end] , title ="m_wage",  label ="")

    println("sum_output: " ,account_a["sum_output"][end] )
    println("sum_capital: " ,account_a["sum_capital"][end] )
    println("employment: " ,account_a["mean_unemploy"][end] )
    println("mean_price: " ,account_a["mean_price"][end] )
    println("mean_consume_w: " ,account_a["mean_consume_w"][end] )
    println("mean_consume_f: " ,account_a["mean_consume_f"][end] )
    println("mean_wage: " ,account_a["mean_wage"][end] )
    println("mean_save_w: " ,account_a["mean_save_w"][end] )
    println("mean_save_fc: " ,account_a["mean_save_fc"][end] )
    println("m_invest_rate_f: " ,account_a["mean_save_fi"][end] )

    display(plot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10))
end

function show_time_generation(account_a)
    # time series macro data
    println("show time")
    waste_b = 1
    println(".")

    p1 = plot(account_a["sum_output"][waste_b:end] , title ="s_output",  label ="")
    p2 = plot(account_a["sum_capital"][waste_b:end] , title ="s_capital",  label ="")
    p3 = plot(account_a["mean_unemploy"][waste_b:end] , title ="m_employ",  label ="")
    p4 = plot(account_a["mean_price"][waste_b:end] , title ="m_price",  label ="")

    p5 = plot(account_a["mean_consume_w"][waste_b:end] , title ="m_consume_w",  label ="")
    p6 = plot(account_a["mean_consume_f"][waste_b:end] , title ="m_consume_f",  label ="")
    p7 = plot(account_a["mean_save_w"][waste_b:end] , title ="m_save_w",  label ="")
    p8 = plot(account_a["mean_save_fi"][waste_b:end] , title ="m_invest_f",  label ="")
    p9 = plot(account_a["mean_save_fc"][waste_b:end] , title ="m_save_f",  label ="")
    p10 = plot(account_a["mean_wage"][waste_b:end] , title ="m_wage",  label ="")

    println("sum_output: " ,account_a["sum_output"][end] )
    println("sum_capital: " ,account_a["sum_capital"][end] )
    println("employment: " ,account_a["mean_unemploy"][end] )
    println("mean_price: " ,account_a["mean_price"][end] )
    println("mean_consume_w: " ,account_a["mean_consume_w"][end] )
    println("mean_consume_f: " ,account_a["mean_consume_f"][end] )
    println("mean_wage: " ,account_a["mean_wage"][end] )
    println("mean_save_w: " ,account_a["mean_save_w"][end] )
    println("mean_save_fc: " ,account_a["mean_save_fc"][end] )
    println("m_invest_f: " ,account_a["mean_save_fi"][end] )

    display(plot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10))
end

function show_time_generation_rep(account_a)
    # time series macro data
    Plots.scalefontsizes(.6)
    println("show time")
    waste_b = 1
    println(".")

    yMin_s = 0
    yMax_s = .65

    p1 = plot(account_a["sum_output"][waste_b:end] , title ="Output",  label ="")
    p2 = plot(account_a["sum_capital"][waste_b:end] , title ="Capital",  label ="")
    p3 = plot(account_a["mean_price"][waste_b:end] , title ="Price",  label ="")

    p4 = plot(account_a["mean_consume_f"][waste_b:end] , title ="Consume",  label ="")
    p5 = plot(account_a["mean_invest_f"][waste_b:end] , title ="Invest",  label ="")
    p6 = plot(account_a["mean_save_fi"][waste_b:end] , title ="invest_rate",  label ="")
    p7 = plot(account_a["mean_save_fc"][waste_b:end] , title ="cash_save",  label ="")

    println("sum_output: " ,account_a["sum_output"][end] )
    println("sum_capital: " ,account_a["sum_capital"][end] )
    println("mean_price: " ,account_a["mean_price"][end] )
    println("mean_consume_f: " ,account_a["mean_consume_f"][end] )
    println("mean_invest_f: " ,account_a["mean_invest_f"][end] )
    println("mean_save_fc: " ,account_a["mean_save_fc"][end] )
    println("m_invest_rate: " ,account_a["mean_save_fi"][end] )

    display(plot(p1,p2,p3,p4,p5,p6,p7))
    Plots.scalefontsizes(1/.6)
end

function show_time_generation_rep_c(account_a,n)
    # time series macro data
    Plots.scalefontsizes(.5)
    println("show time")
    periods = length(account_a["sum_output"])
    waste_a = 16
    waste_b = 10
    println(".")

    yMin_o = 300
    yMax_o = 4000

    yMin_c = 1000
    yMax_c = 2000

    yMin_p = 0
    yMax_p = 3

    yMin_ci = .4
    yMax_ci = .17

    yMin_i = 0
    yMax_i = .26

    yMin_s = 0
    yMax_s = .9

    #=
    p1 = plot(account_a["sum_output"][waste_a:end-waste_b] , title ="Output",  label ="")
    p2 = plot(account_a["sum_capital"][waste_a:end-waste_b] , title ="Capital",  label ="")
    p3 = plot(account_a["mean_price"][waste_a:end-waste_b] , title ="Price",  label ="")
    p4 = plot(1:periods+1-waste_b-waste_a, [account_a["mean_consume_f"][waste_a:end-waste_b] account_a["mean_invest_f"][waste_a:end-waste_b]], title ="Consume",  label ="")
    p5 = plot(account_a["mean_save_fi"][waste_a:end-waste_b] , title ="invest_rate",  label ="")
    p6 = plot(account_a["mean_save_fc"][waste_a:end-waste_b] , title ="cash_save",  label ="")
    =#

    #p1 = plot(account_a["sum_output"][waste_a:end-waste_b], ylim=(yMin_o, yMax_o) , title ="Output",  label ="")

    p2 = plot(account_a["sum_capital"][waste_a:end-waste_b], ylim=(yMin_c, yMax_c) , title ="Capital",  label ="")
    p3 = plot(account_a["mean_price"][waste_a:end-waste_b], ylim=(yMin_p, yMax_p) , title ="Price",  label ="")
    p1 = plot(1:periods+1-waste_b-waste_a, [account_a["sum_output"][waste_a:end-waste_b] account_a["mean_consume_f"][waste_a:end-waste_b]*n account_a["mean_invest_f"][waste_a:end-waste_b]*n ], ylim=(yMin_o, yMax_o), title ="Consume Consume Invest",  label ="")
    p4 = plot(account_a["mean_save_fi"][waste_a:end-waste_b], ylim=(yMin_i, yMax_i) , title ="invest_rate",  label ="")
    p5 = plot(account_a["mean_save_fc"][waste_a:end-waste_b], ylim=(yMin_s, yMax_s) , title ="cash_save",  label ="")

    println("sum_output: " ,account_a["sum_output"][end] )
    println("sum_capital: " ,account_a["sum_capital"][end] )
    println("mean_price: " ,account_a["mean_price"][end] )
    println("mean_consume_f: " ,account_a["mean_consume_f"][end] )
    println("mean_invest_f: " ,account_a["mean_invest_f"][end] )
    println("mean_save_fc: " ,account_a["mean_save_fc"][end] )
    println("m_invest_rate: " ,account_a["mean_save_fi"][end] )

    display(plot(p1,p2,p3,p4,p5))
    Plots.scalefontsizes(1/.5)
end

function show_time_generation_rep_b(account_a,n)
    # time series macro data
    Plots.scalefontsizes(.6)
    println("show time")
    periods = length(account_a["sum_output"])
    waste_a = 25
    waste_b = 0
    println(".")

    yMin_o = 500
    yMax_o = 3200

    yMin_c = 1000
    yMax_c = 3250

    yMin_s = 0
    yMax_s = .5


    p1 = plot(waste_a:periods-waste_b,[account_a["sum_output"][waste_a:end-waste_b] account_a["mean_consume_f"][waste_a:end-waste_b]*n], title ="Output & Consumption",  label ="")
    #p1 = plot(waste_b:periods,[account_a["sum_output"][waste_b:end] account_a["mean_consume_f"][waste_b:end]*n], ylim=(yMin_o, yMax_o), title ="Output & Consumption",  label ="")

    p2 = plot(account_a["sum_capital"][waste_a:end-waste_b] , ylim=(minimum(account_a["sum_capital"][waste_a:end-waste_b])*.9, maximum(account_a["sum_capital"][waste_a:end-waste_b])*1.2), title ="Capital",  label ="")
    #p2 = plot(account_a["sum_capital"][waste_a:end] , ylim=(yMin_c, yMax_c), title ="Capital",  label ="")
    #p3 = plot(account_a["mean_save_fi"][waste_a:end-waste_b] , title ="Invest_rate",  label ="")

    p3 = plot(account_a["mean_price"][waste_a:end-waste_b] , ylim=(minimum(account_a["mean_price"][waste_a:end-waste_b])*.9, maximum(account_a["mean_price"][waste_a:end-waste_b])*1.2), title ="Price",  label ="")
    #p3 = plot(account_a["mean_save_fi"][waste_a:end] , ylim=(yMin_s, yMax_s), title ="Invest_rate",  label ="")
    p4 = plot(account_a["mean_save_fi"][waste_a:end] , ylim=(yMin_s, yMax_s), title ="Invest_rate",  label ="")
    p5 = plot(account_a["mean_save_fc"][waste_a:end] , ylim=(0, .5), title ="Cash Save",  label ="")

    p6 = plot(account_a["mean_save_fi"][waste_a:end] , ylim=(0, 1), title ="Invest_rate",  label ="")
    p7 = plot(account_a["mean_save_fi"][waste_a:end] , ylim=(0, 2), title ="Invest_rate",  label ="")
    p8 = plot(account_a["mean_save_fi"][waste_a:end] , ylim=(0, 3), title ="Invest_rate",  label ="")
    p9 = plot(account_a["mean_save_fi"][waste_a:end] , ylim=(0, 4), title ="Invest_rate",  label ="")
    p10 = plot(account_a["mean_save_fi"][waste_a:end] , ylim=(0, 5), title ="Invest_rate",  label ="")
    p11 = plot(account_a["mean_save_fi"][waste_a:end] , ylim=(0, 5), title ="Invest_rate",  label ="")

    println("sum_output: " ,account_a["sum_output"][end] )
    println("sum_capital: " ,account_a["sum_capital"][end] )
    println("mean_price: " ,account_a["mean_price"][end] )
    println("mean_consume_f: " ,account_a["mean_consume_f"][end] )
    println("mean_invest_f: " ,account_a["mean_invest_f"][end] )
    println("mean_save_fc: " ,account_a["mean_save_fc"][end] )
    println("m_invest_rate: " ,account_a["mean_save_fi"][end] )

    #display(plot(p1,p6,p2,p7,p3,p8,p4,p9,p5,p10,p11, layout=(5,3)))
    display(plot(p3,p3,p1,p2,p4,p5, layout=(3,2) ))

    Plots.scalefontsizes(1/.6)
end

function show_time_generation_full(account_a)
    # time series macro data
    Plots.scalefontsizes(.6)
    println("show time")
    waste_a = 251
    waste_b = 0
    println(".")

    yMin_s = 0
    yMax_s = .6

    p1 = plot(account_a["sum_output"][waste_a:end-waste_b] , ylim=(minimum(account_a["sum_output"][waste_a:end-waste_b])*.9, maximum(account_a["sum_output"][waste_a:end-waste_b])*1.1), title ="Output",  label ="")
    p2 = plot(account_a["sum_capital"][waste_a:end-waste_b], ylim=(minimum(account_a["sum_capital"][waste_a:end-waste_b])*.9, maximum(account_a["sum_capital"][waste_a:end-waste_b])*1.1) , title ="Capital",  label ="")
    p3 = plot(account_a["mean_price"][waste_a:end-waste_b], ylim=(minimum(account_a["mean_price"][waste_a:end-waste_b])*.9, maximum(account_a["mean_price"][waste_a:end-waste_b])*1.1) , title ="Price",  label ="")
    p4 = plot(account_a["mean_unemploy"][waste_a:end-waste_b] , title ="Employed",  label ="")

    p5 = plot(account_a["mean_consume_f"][waste_a:end-waste_b] , ylim=(minimum(account_a["mean_consume_f"][waste_a:end-waste_b])*.9, maximum(account_a["mean_consume_f"][waste_a:end-waste_b])*1.1), title ="Entrep_Cons",  label ="")
    p6 = plot(account_a["mean_invest_f"][waste_a:end-waste_b] , ylim=(minimum(account_a["mean_invest_f"][waste_a:end-waste_b])*.9, maximum(account_a["mean_invest_f"][waste_a:end-waste_b])*1.1), title ="Entrep_Invst",  label ="")
    p7 = plot(account_a["mean_save_fi"][waste_a:end-waste_b] , ylim=(yMin_s, maximum(account_a["mean_save_fi"][waste_a:end-waste_b])*1.2), title ="Invest_rate",  label ="")
    p8 = plot(account_a["mean_save_fc"][waste_a:end-waste_b] , ylim=(yMin_s, maximum(account_a["mean_save_fc"][waste_a:end-waste_b])*1.2), title ="E_cash_save_r",  label ="")

    p9 = plot(account_a["mean_consume_w"][waste_a:end-waste_b], ylim= (minimum(account_a["mean_consume_w"][waste_a:end-waste_b])*.9, maximum(account_a["mean_consume_w"][waste_a:end-waste_b])*1.1) , title ="Worker_Cons",  label ="")
    p10 = plot(account_a["mean_wage"][waste_a:end-waste_b], ylim= (minimum(account_a["mean_wage"][waste_a:end-waste_b])*.9, maximum(account_a["mean_wage"][waste_a:end-waste_b])*1.1) , title ="Wage",  label ="")
    p11 = plot(account_a["mean_save_w"][waste_a:end-waste_b] , ylim= (minimum(account_a["mean_save_w"][waste_a:end-waste_b])*.9, maximum(account_a["mean_save_w"][waste_a:end-waste_b])*1.1), title ="Worker_save",  label ="")
    p12 = plot(account_a["worker_move"][waste_a:end-waste_b] , title ="Worker_New_Job",  label ="")

    println("sum_output: " ,account_a["sum_output"][end] )
    println("sum_capital: " ,account_a["sum_capital"][end] )
    println("mean_price: " ,account_a["mean_price"][end] )
    println("employment: " ,account_a["mean_unemploy"][end] )
    println("worker_move: " ,account_a["worker_move"][end] )
    println("mean_consume_w: " ,account_a["mean_consume_w"][end] )
    println("mean_consume_f: " ,account_a["mean_consume_f"][end] )
    println("mean_invest_f: " ,account_a["mean_invest_f"][end] )
    println("m_invest_rate: " ,account_a["mean_save_fi"][end] )
    println("f_cash_save: " ,account_a["mean_save_fc"][end] )
    println("w_cash_save: " ,account_a["mean_save_w"][end] )

    println("Numbers time")
    println()
    println("output change: ", maximum(account_a["sum_output"][waste_a:end-waste_b]) / minimum(account_a["sum_output"][waste_a:end-waste_b]) )
    println("capital change: ", maximum(account_a["sum_capital"][waste_a:end-waste_b]) / minimum(account_a["sum_capital"][waste_a:end-waste_b]) )
    println("price change: ", maximum(account_a["mean_price"][waste_a:end-waste_b]) / minimum(account_a["mean_price"][waste_a:end-waste_b]) )
    println("wage change: ", maximum(account_a["mean_wage"][waste_a:end-waste_b]) / minimum(account_a["mean_wage"][waste_a:end-waste_b]) )
    println("min employment: ",  minimum(account_a["mean_unemploy"][waste_a:end-waste_b]) )
    println("max turnover: ",  maximum(account_a["worker_move"][waste_a:end-waste_b]) )

    display(plot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12))
    Plots.scalefontsizes(1/.6)
end

function show_cross_full(state, p_account)
    println()
    println("mean_output: " , mean(p_account["output"]) )
    println("share_sold: " , p_account["share_sold"] )
    println("mean price: " , mean(p_account["m_price"]) )
    println("sd price: " , std(p_account["m_price"]) )
    wage = sum(state["wage_net"], dims = 1)
    n_employee = sum((state["wage_net"].> 0), dims = 2)
    println("mean wage: " , mean(wage) )
    println("sd wage: " , std(wage) )
    Plots.scalefontsizes(.6)

    pp1 = histogram(p_account["output"], bins = 20, title="Output", label ="" )
    pp2 = histogram(state["capital"], bins = 20, title="Capital", label ="" )
    pp3 = scatter(state["productivity"], state["capital"], markersize=2, title=" K / Z", label ="" )

    pp4 = histogram(p_account["firm_invest_rate"], bins = 20, title="Invest_rate", label ="" )
    pp5 = scatter(state["productivity"], p_account["firm_invest_rate"], markersize=2, title=" Invest_r / Z", label ="" )
    pp6 = scatter(state["capital"], p_account["firm_invest_rate"], markersize=2, title=" Invest_r / K", label ="" )

    pp7 = histogram(p_account["firm_consume_rate"], bins = 20, title="cash_save", label ="" )
    #pp8 = scatter(state["productivity"], n_employee, markersize=2, title=" employees / Z", label ="" )
    pp8 = histogram(n_employee, bins = maximum(n_employee)+1, title="Employees", label ="" )
    pp9 = histogram(p_account["firm_consume_level"], bins = 20, title="f_consume", label ="" )

    pp10 = histogram(p_account["worker_save_rate"], bins = 20, title="worker_cash_save", label ="" )
    pp11 = histogram(wage', bins = 20, title="wage", label ="" )
    pp12 = histogram(p_account["worker_consume_level"], bins = 20, title="w_consume", label ="" )

    display(plot(pp1,pp2,pp3,pp4,pp5,pp6, pp7,pp8, pp9, pp10, pp11, pp12,   layout=(4,3)))
    Plots.scalefontsizes(1/.6)
end

function numbers_full(account_a)
    waste_a = 226
    waste_b = 0
    println()

    println("Numbers time")
    println()
    println("output change: ", maximum(account_a["sum_output"][waste_a:end-waste_b]) / minimum(account_a["sum_output"][waste_a:end-waste_b]) )
    println("capital change: ", maximum(account_a["sum_capital"][waste_a:end-waste_b]) / minimum(account_a["sum_capital"][waste_a:end-waste_b]) )
    println("price change: ", maximum(account_a["mean_price"][waste_a:end-waste_b]) / minimum(account_a["mean_price"][waste_a:end-waste_b]) )
    println("wage change: ", maximum(account_a["mean_wage"][waste_a:end-waste_b]) / minimum(account_a["mean_wage"][waste_a:end-waste_b]) )
    println("min employment: ",  minimum(account_a["mean_unemploy"][waste_a:end-waste_b]) )
    println("max turnover: ",  maximum(account_a["worker_move"][waste_a:end-waste_b]) )
end

function show_cross_b(state, p_account)

    title = plot(title = "One Generation", grid = false, showaxis = false, bottom_margin = -25Plots.px)
    pp1 = histogram(state["capital"], bins = 10, title="Capital", label ="", xaxis = ("my label", (0,.5), 0:.25:4, font(10, "Courier")) )
    # xaxis = ("my label", (0,1), 0:.25:4, font(10, "Courier"))  (0,1) from 0 to 1 next is tep
    wage = sum(state["wage_net"], dims = 1)
    pp2 = histogram(wage', bins = 10, title="wage", label ="" )
    display(plot(title, pp1, pp2, layout = @layout([A{0.01h}; [B C]])))
    # histograms
end


function show_cross_rep(state, p_account)
    # for growth model frition
    println()
    println("mean_output: " , mean(p_account["output"]) )
    println("share_sold: " , p_account["share_sold"] )
    println("mean price: " , mean(p_account["m_price"]) )
    println("sd price: " , std(p_account["m_price"]) )
    Plots.scalefontsizes(.6)

    pp1 = histogram(p_account["output"], bins = 10, title="Output", label ="" )
    pp2 = histogram(state["capital"], bins = 10, title="Capital", label ="" )
    pp3 = scatter(state["productivity"], state["capital"], markersize=2, title=" K / Z", label ="" )

    pp4 = histogram(p_account["firm_invest_rate"], bins = 10, title="Invest_rate", label ="" )
    pp5 = scatter(state["productivity"], p_account["firm_invest_rate"], markersize=2, title=" Invest_r / Z", label ="" )
    pp6 = scatter(state["capital"], p_account["firm_invest_rate"], markersize=2, title=" Invest_r / K", label ="" )

    pp7 = histogram(p_account["firm_consume_rate"], bins = 10, title="cash_save", label ="" )
    pp8 = scatter(state["productivity"], p_account["firm_consume_rate"], markersize=2, title=" cash_s / Z", label ="" )
    pp9 = scatter(state["capital"], p_account["firm_consume_rate"], markersize=2, title=" cash_s / K", label ="" )

    pp10 = histogram(p_account["m_price"], bins = 10, title="Price", label ="" )
    pp11 = plot(account_para["para_f_invest"] , title ="para_invest",  label ="")
    pp12 = plot(account_para["para_f_consume"] , title ="para_save",  label ="")


    display(plot(pp1,pp2,pp3,pp4,pp5,pp6, pp7,pp8, pp9, pp10, pp11, pp12,   layout=(4,3)))
    Plots.scalefontsizes(1/.6)
end


function show_para(account_para)
    # time series macro data
    waste_b = 1

    p1 = plot(account_para["para_f_consume"] , title ="f_consume",  label ="")
    p2 = plot(account_para["para_f_invest"] , title ="f_invest",  label ="")
    p3 = plot(account_para["para_w_consume"] , title ="w_consume",  label ="")
    display(plot(p1,p2,p3))
end

function show_3d_decsion_two(e_para, max_x, max_y, sim_set, p_title)
    x=range(0,stop=max_x,length=100)
    y=range(0,stop=max_y,length=100)

    println()
    println(x)
    println(y)

    function q_eval_a(xx,yy)  # evaluate the function to be ploted
        input = hcat(xx, yy)
        ss = q_choice_g_two(input, e_para, sim_set["scale"])
        return ss[1]
    end
    #display(plot(x,y,q_eval_a,st=:surface,camera=(-10,30)))
    pa = plot(x,y,q_eval_a,st=:surface,camera=(-10,30), title = p_title)
    return pa
end

function show_3d_decsion_three(e_para, mean_z, max_x, max_y, sim_set, p_title)
    x=range(0,stop=max_x,length=100)
    y=range(0,stop=max_y,length=100)
    function q_eval_a(xx,yy)  # evaluate the function to be ploted
        input = hcat(mean_z, xx, yy)
        ss = q_choice_g_three(input, e_para, sim_set["scale"])
        return ss[1]
    end
    #display(plot(x,y,q_eval_a,st=:surface,camera=(-10,30)))
    println("first input fixed at: ", mean_z)
    pa = plot(x,y,q_eval_a,st=:surface,camera=(-10,30), title = p_title)
    return pa
end
