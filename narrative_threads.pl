

/************** STORY WORLD DATA FOR THE MAFIA STORIES ****************/


/* NARRATIVE THREADS  */

arcdata(enters_home1,2,[wants_to_go_home,drives_home1,drives_home2,drives_home3,arrives_home,turns_off_motor,gets_out_of_car,leaves_car,enters_home]).

arcdata(gianni_gets_killed1,-10,[wants_to_kill_gianni,plants_bomb_in_car,checks_gianni_gets_in_car,triggers_remote_control,countdown_starts,countdown_starts_in_car,countdown_goes_on,countdown_goes_to_end,bomb_explodes,gianni_gets_killed]).
arcdata(countdown_fails,1,[countdown_starts_in_car,countdown_goes_on,countdown_fails]).
arcdata(things_get_blown_up,-5,[bomb_explodes,things_get_blown_up]).
arcdata(people_get_blown_up,-7,[bomb_explodes,people_get_blown_up]).

arcdata(gianni_tracked,-2,[wants_track_gianni, checks_gianni_gets_in_car,  triggers_remote_control, map_on_remote_shows_giannis_position, gianni_tracked]).

arcdata(gianni_surprised,4,[wants_surprise_gianni , checks_gianni_gets_in_car , calls_friends , car_goes_to_surprise_point,friends_jump_out_on_gianni, gianni_surprised]).

/************/

arcdata(car_breaks_down,-2,[mechanical_problem_with_car,car_breaks_down]). 
arcdata(resolves_mechanical_problem,2,[part_of_car_was_loose,car_goes_on_bumpy_road2,car_gets_shaken2,mechanical_problem_with_car,strange_noise_from_car ,
hears_noise_from_car, wants_to_find_noise, stops_car1,goes_towards_noise, sees_something, sees_mechanical_problem,resolves_mechanical_problem]). 

arcdata(sees_only_things_blown_up,-4,[bomb_explodes, things_get_blown_up,
hears_a_big_bang, wants_to_see_source_of_bang,goes_towards_bang, sees_only_things_blown_up]). 

arcdata(sees_people_and_things_blown_up,-6,[bomb_explodes, people_get_blown_up,
hears_a_big_bang, wants_to_see_source_of_bang,goes_towards_bang, sees_people_and_things_blown_up]). 

arcdata(sees_car_damaged_in_accident,-4,[car_damaged_in_accident,hears_a_big_bang, wants_to_see_source_of_bang,goes_towards_bang, sees_car_damaged_in_accident]). 

arcdata(gianni_gets_killed3,-10,[wants_to_kill_gianni,plants_bomb_in_car,checks_gianni_gets_in_car,car_goes_on_bumpy_road1,car_gets_shaken1,bomb_in_car_gets_shaken,bomb_explodes,gianni_gets_killed]).


arcdata(gets_away_from_bomb,6,[bomb_in_car_gets_shaken,bomb_in_car_changes_behaviour,strange_noise_from_car,hears_noise_from_car,wants_to_find_noise,stops_car1,goes_towards_noise,sees_something,sees_bomb,gets_away_from_bomb]).

arcdata(gianni_gets_killed3,-10,[plants_bomb_in_car,bomb_in_car_changes_behaviour,strange_noise_from_car,ignition_gets_triggered,bomb_explodes,gianni_gets_killed]).

arcdata(decides_and_enters_home,1,[wants_to_know_what_to_do,thinks_about_alternative1,thinks_about_alternative2,thinks_about_alternative3,decides_what_to_do,gets_out_of_car,leaves_car,enters_home]).

arcdata(car_damaged,1,[car_crashes,car_damaged_in_accident,hears_a_big_bang]). 

/*—————— IMPORTANCE VALUES FOR EACH THREAD -----------------------*/

outcomeval(gianni_gets_killed,-10).
outcomeval(discovers_noise_source,2).
outcomeval(enters_home,2).
outcomeval(knows_what_to_do,1).
outcomeval(things_get_blown_up,-6).
outcomeval(people_get_blown_up,-9).
outcomeval(_,0).

/*—————— D: SET OF DISALLOWING PAIRS -----------------------*/
	
disallow(A,B):- disallowtwo(A,B).
disallow(A,B):- disallowtwo(B,A).
		
disallow(wants_to_know_what_to_do,gets_out_of_car).
disallow(stops_car1,bomb_in_car_gets_shaken).
disallow(stops_car1,drives_home2).
disallow(turns_off_motor,bomb_in_car_gets_shaken).

disallow(resolves_mechanical_problem,mechanical_problem_with_car).

disallow(resolves_mechanical_problem,car_breaks_down).

disallow(sees_only_things_blown_up,sees_people_and_things_blown_up).

disallow(drives2,stops_car1).

/****** RECIPROCAL DISALLOWING PAIRS ******/


disallowtwo(countdown_fails,bomb_explodes).
disallowtwo(sees_mechanical_problem, sees_bomb).
disallowtwo(gets_away_from_bomb,gianni_gets_killed). 
disallowtwo(leaves_car,gianni_gets_killed). 

disallowtwo(wants_track_gianni,wants_surprise_gianni).
disallowtwo(wants_to_kill_gianni,wants_surprise_gianni).
disallowtwo(wants_to_kill_gianni,wants_track_gianni).

disallowtwo(car_breaks_down,drives_home3). 

disallowtwo(part_of_car_was_loose,plants_bomb_in_car).

disallowtwo(mechanical_problem_with_car,bomb_in_car_gets_shaken).

disallowtwo(mechanical_problem_with_car,bomb_in_car_changes_behaviour).

disallowtwo(car_goes_on_bumpy_road2,bomb_in_car_gets_shaken).

disallowtwo(car_gets_shaken2,bomb_in_car_gets_shaken).

disallowtwo(triggers_remote_control,calls_friends).

disallowtwo(countdown_starts,map_on_remote_shows_giannis_position). 

disallowtwo(bomb_explodes,car_damaged_in_accident). 

disallowtwo(sees_only_things_blown_up,sees_car_damaged_in_accident).  
   
disallowtwo(sees_people_and_things_blown_up,sees_car_damaged_in_accident).


