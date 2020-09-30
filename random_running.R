### create function to come up with runs
### want to do about 50 miles per week, run 6 ish times a week, at varying paces
library(googlesheets4)
gs4_auth(email = "eric.thiel96@gmail.com")
existing = read_sheet("https://docs.google.com/spreadsheets/d/1wvxYJrN2jjSGHe9DrLNaQqZJKeJzX5tA_txFpmkv5H8/edit#gid=0",
                   "Log")


generate_run = function(){
distance = round(runif(1, 4, 12),2)
mean_distance = 8
pace = round(runif(1, 7.25, 9),2)
mean_pace = 8

pace_mod = sqrt(sqrt(sqrt(distance / mean_distance)))
pace = round(pace * pace_mod,2)

decimal <- pace - floor(pace)
decimal = round(decimal * 0.6,2)
non_decimal = floor(pace)



pace_in_time = decimal + non_decimal

workout = list(distance, pace_in_time)

return(workout)
}

g = generate_run()

Distance = g[[1]]
Pace = g[[2]]

Date = Sys.Date()


df = data.frame(Date, Distance, Pace)

sheet_append("https://docs.google.com/spreadsheets/d/1wvxYJrN2jjSGHe9DrLNaQqZJKeJzX5tA_txFpmkv5H8/edit#gid=0", df, sheet = "Log")










