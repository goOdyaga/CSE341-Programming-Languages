:- dynamic delivery_person/5.
:- dynamic delivery_object/7.


% Define delivery personnel
delivery_person(person1, capacity(50), work_hours(9, 17), current_job([]), location(library)).
delivery_person(person2, capacity(42), work_hours(8, 16), current_job([]), location(library)).
delivery_person(person3, capacity(35), work_hours(6, 22), current_job([]), location(cafeteria)).

% Define places
place(library, id1).
place(admin_office, id3).
place(engineering_bld, id4).
place(lecture_hall_a, id5).
place(institue_y, id6).
place(cafeteria, id7).
place(social_science_bld, id8).
place(institue_x, id9).

% Define objects for delivery
delivery_object(package1, weight(5),deliver_id(none), pickup(library), dropoff(lecture_hall_a), urgency(high),situation(none)).
delivery_object(package2, weight(6),deliver_id(none), pickup(library), dropoff(engineering_bld), urgency(low),situation(none)).
delivery_object(package3, weight(4),deliver_id(none), pickup(cafeteria), dropoff(institue_y), urgency(mediom),situation(none)).
delivery_object(package4, weight(3),deliver_id(none), pickup(social_science_bld), dropoff(lecture_hall_a), urgency(high),situation(none)).
delivery_object(package5, weight(8),deliver_id(none), pickup(institue_x), dropoff(admin_office), urgency(high),situation(none)).


% Define routes
route(library, admin_office, deliver_time(1)).
route(admin_office, library, deliver_time(1)).

route(library, engineering_bld, deliver_time(5)).
route(engineering_bld, library, deliver_time(5)).

route(library, institue_y, deliver_time(3)).
route(institue_y, library, deliver_time(3)).

route(library, admin_office, deliver_time(1)).
route(admin_office, library, deliver_time(1)).

route(library, cafeteria, deliver_time(5)).
route(cafeteria, library, deliver_time(5)).

route(library, social_science_bld, deliver_time(2)).
route(social_science_bld, library, deliver_time(2)).

route(lecture_hall_a, institue_y, deliver_time(3)).
route(institue_y, lecture_hall_a, deliver_time(3)).

route(cafeteria, social_science_bld, deliver_time(2)).
route(social_science_bld, cafeteria, deliver_time(2)).

route(social_science_bld, institue_x, deliver_time(8)).
route(institue_x, social_science_bld, deliver_time(8)).


% Base case: reached the destination.
path(Start, Start, Visited, [], 0) :-
    reverse(Visited, _).

% Recursive case: find a path from Start to End.
path(Start, End, Visited, [Next|RestOfPath], TotalTime) :-
    Start \= End,
    route(Start, Next, deliver_time(TimeToNext)),
    \+ member(Next, Visited),  % Prevent visiting the same node
    path(Next, End, [Next|Visited], RestOfPath, TimeRestOfPath),
    TotalTime is TimeToNext + TimeRestOfPath.

% Wrapper to start the pathfinding without visited nodes.
find_path(Start, End, Path, TotalTime) :-
    path(Start, End, [Start], Path, TotalTime).

% Print the path and total time.
print_path_and_time(Path, TotalTime) :-
    write('Path: '), print_path(Path), nl,
    write('Total Time: '), write(TotalTime), nl.

% Helper predicate to print the path.
print_path([]).
print_path([H|T]) :-
    write(H), 
    (T \= [] -> write(' -> '); true), 
    print_path(T).

% Start the program, find the path, and print.
start(Start, End) :-
    find_path(Start, End, path, totalTime).


assign_package(Package, Person) :-
    delivery_person(Person, capacity(MaxCapacity), work_hours(StartHour, EndHour), current_job(CurrentJobs), location(Location)),
    findall(Weight, (member(Pkg, CurrentJobs), delivery_object(Pkg, weight(Weight), _, _, _, _, _)), Weights),
    sum_list(Weights, TotalWeight),
    delivery_object(Package, weight(PackageWeight), _, pickup(Pickup), dropoff(Dropoff), urgency(Urgency), situation(none)),
    NewTotalWeight is TotalWeight + PackageWeight,
    (NewTotalWeight =< MaxCapacity  ->
        (CurrentJobs == [] ->
            Location == Pickup
        ;  
        
            last(CurrentJobs, LastPackage),
            delivery_object(LastPackage, _, _, _, dropoff(LastDropoff), _, _),
            % Checking if new package is on the way or can be added to the route
            ( find_path(LastDropoff, Dropoff, Path, _),
              member(Pickup, Path) -> true
            ; find_path(LastDropoff, Pickup, _, TimeToNextPickup),
              find_path(Pickup, Dropoff, _, TimeToNextDropoff),
              TotalTime is TimeToNextPickup + TimeToNextDropoff,
              TotalTime =< (EndHour - StartHour)
            )
        ),
     

        retract(delivery_object(Package, weight(PackageWeight), _, pickup(Pickup), dropoff(Dropoff), _, _)),
        assert(delivery_object(Package, weight(PackageWeight), deliver_id(Person), pickup(Pickup), dropoff(Dropoff), urgency(Urgency), situation(transit))),
        % Updating the delivery persons jobs
        retract(delivery_person(Person, capacity(MaxCapacity), work_hours(StartHour, EndHour), current_job(CurrentJobs), location(Location))),
        assert(delivery_person(Person, capacity(MaxCapacity), work_hours(StartHour, EndHour), current_job([Package|CurrentJobs]), location(Location)))
    ;   write('Cannot assign package: Exceeds capacity.'), nl, fail).


can_assign(Package, Person) :-
    delivery_person(Person, capacity(MaxCapacity), work_hours(StartHour, EndHour), current_job(CurrentJobs), location(Location)),
    findall(Weight, (member(Pkg, CurrentJobs), delivery_object(Pkg, weight(Weight), _, _, _, _, _)), Weights),
    sum_list(Weights, TotalWeight),
    delivery_object(Package, weight(PackageWeight), _, pickup(Pickup), dropoff(Dropoff), _, situation(none)),
    NewTotalWeight is TotalWeight + PackageWeight,
    (NewTotalWeight =< MaxCapacity  ->
        (CurrentJobs == [] ->
            Location == Pickup
        ;  
        
            last(CurrentJobs, LastPackage),
            delivery_object(LastPackage, _, _, _, dropoff(LastDropoff), _, _),
            % Checking if new package is on the way or can be added to the route
            ( find_path(LastDropoff, Dropoff, Path, _),
              member(Pickup, Path) -> true
            ; find_path(LastDropoff, Pickup, _, TimeToNextPickup),
              find_path(Pickup, Dropoff, _, TimeToNextDropoff),
              TotalTime is TimeToNextPickup + TimeToNextDropoff,
              TotalTime =< (EndHour - StartHour)
            )
        )
     

    ;   write('Cannot assign package: Exceeds capacity.'), nl, fail).



print_package_info(Package) :-
    delivery_object(Package, Weight, DeliverID, pickup(Pickup), dropoff(Dropoff), Urgency, Situation),
    format('Package: ~w~n', [Package]),
    format('Weight: ~w~n', [Weight]),
    format('Delivery ID: ~w~n', [DeliverID]),
    format('Pickup Location: ~w~n', [Pickup]),
    format('Dropoff Location: ~w~n', [Dropoff]),
    format('Urgency: ~w~n', [Urgency]),
    format('Situation: ~w~n', [Situation]).

print_delivery_person_info(Person) :-
    write(''),nl,
    delivery_person(Person, Capacity, work_hours(StartHour, EndHour), current_job(CurrentJobs), Location),
    format('Delivery Person: ~w~n', [Person]),
    format('Capacity: ~w~n', [Capacity]),
    format('Work Hours: ~w to ~w~n', [StartHour, EndHour]),
    format('Current Location: ~w~n', [Location]),
    write(''),nl,
    write('JOBS'),nl,
     
    print_jobs_path(CurrentJobs),
    write(''),nl.

print_jobs_path([]) :- !.

print_jobs_path([Job|Rest]) :-
    delivery_object(Job, weight(Weight), deliver_id(_), pickup(Pickup), dropoff(Dropoff), urgency(Urgency), situation(Situation)),
    format('Job: ~w, Weight: ~w, Pickup: ~w, Dropoff: ~w, Urgency: ~w, Situation: ~w~n', [Job, Weight, Pickup, Dropoff, Urgency, Situation]),
    find_path(Pickup, Dropoff, Path, TotalTime),
    format('Path: ~w, Total Time: ~w~n', [Path, TotalTime]),
    print_jobs_path(Rest).


check(Package) :-
    delivery_object(Package, _, deliver_id(Deliver_id), _, _, urgency(_), situation(Situation)),
    % Check if the package is in 'transit' situation.
    (Situation == transit -> 
        format('Package is currently in transit, delivered by: ~w~n', [Deliver_id]),
        true
    ; 
        % Check for available delivery persons.
        findall(Person, delivery_person(Person, _, _, _, _), AllDeliveryPersons),
       
        check_available_delivery_persons(Package,  AllDeliveryPersons)
    ).



check_available_delivery_persons(_, []) :- !.

check_available_delivery_persons(Package, [Person|Rest]) :-
    (can_assign(Package, Person) ->
        print_delivery_person_info(Person)
    ; true),
    check_available_delivery_persons(Package, Rest).
