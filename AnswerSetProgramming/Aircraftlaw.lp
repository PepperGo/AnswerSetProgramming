%%%%%%%%%%%%%%%%% Definitions %%%%%%%%%%%%%%%%%%%%%%%%
% (1) "Aircraft" means a device that is invented, used, or designated for air navigation or flight, 
% other than a parachute or other device used primarily as safety equipment.

% Aircraft(A) is true if a device A satisfies the aircraf definiton.

aircraft(Device):- not safety_equipment(Device), 
			       air_navigation(Device).

aircraft(Device):- not safety_equipment(Device), 
			       flight(Device).	 

%-aircraft(Device):- safety_equipment(Device).
%-aircraft(Device):- not aircraft(Device).

% (2)  "Airman" means:
% (A)  a person, including the person in command of an aircraft or a pilot, mechanic, or member of the crew, 
%      who engages in the navigation of an aircraft while under way;  or
% (B)  the person who is in charge of the inspection, overhaul, or repair of an aircraft.

% airman(P) is true if a person(P) satisfies the airman defition.

person(P).
airman(P):- person(P), in_command(P).
airman(P):- person(P), pilot(P).
airman(P):- person(P), mechanic(P).
airman(P):- person(P), crew(P).
airman(P):- person(P), in_charge(P).


%%%%%%%%%%%%%%%%%%% Offense %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%% OPERATION OF AIRCRAFT WITHOUT AIRMAN CERTIFICATE;  OFFENSE. %%%%%%%%%%%%%%%%%%%%
% This subchapter does not apply to an aircraft owned by and used exclusively in the service of the federal or state government.

% (3)  "Airman certificate" means a certificate issued to an airman under 49 U.S.C. Section 1422.

airman_certificate(P):- airman(P), has_certificate(P).
%-airman_certificate(P):- not airman_certificate(P).


%-cetificate_offense(P, A):- person(P), aircraft(A), 
							airman_certificate(P).

%-cetificate_offense(P, A):- person(P), aircraft(A), 
							by_government(A).

%cetificate_offense(P, A):- not -cetificate_offense(P, A).

cetificate_offense(P, A):- person(P), aircraft(A), 
						   not airman_certificate(P).

cetificate_offense(P, A):- person(P), aircraft(A), 
						   not by_government(A).	

%%%%%%%%%%%%%%%%% FAILURE TO REGISTER AIRCRAFT;  OFFENSE. %%%%%%%%%%%%%%%%%%
aircraft_register(A):- aircraft(A), register(A).
%-aircraft_register(A):- aircraft(A), not aircraft_register(A).

%-aircraft_register_offense(A):- aircraft(A), aircraft_register(A).

%aircraft_register_offense(A):- aircraft(A), not -aircraft_register_offense(A).
aircraft_register_offense(A):- aircraft(A), not aircraft_register(A).

%%%%%%%%%%%%%%%% AIRCRAFT IDENTIFICATION NUMBERS;  OFFENSE. %%%%%%%%%%%%%%%%%%
aircraft_hasID(A):- aircraft(A), id_number(ANum).
%-aircraft_hasID(A):- not aircraft_hasID(A).

%-aircraft_ID_offense(A):- aircraft(A), aircraft_hasID(A).
%aircraft_ID_offense(A):- aircraft(A), not -aircraft_ID_offense(A).
aircraft_ID_offense(A):- aircraft(A), not aircraft_hasID(A).

%%%%%%%%%%%%%%%%% AIRCRAFT FUEL CONTAINERS;  OFFENSE. %%%%%%%%%%%%%%%%%%%%%%
% (1) a fuel container
fuel_container_approved(A):- aircraft(A), fuel_container_permit(A).
fuel_container_approved(A):- aircraft(A), fuel_container_inspection(A).
%-fuel_container_approved(A):- aircraft(A), not fuel_container_approved(A).

fuel_container_ok(A):- aircraft(A), fuel_container_conform(A).
fuel_container_ok(A):- aircraft(A),	fuel_container_approved(A).
%-fuel_container_ok(A):- aircraft(A), not fuel_container_ok(A).

%-fuel_container_offense(A):- aircraft(A), fuel_container_ok(A).
%fuel_container_offense(A):- aircraft(A), not -fuel_container_offense(A).
fuel_container_offense(A):- aircraft(A), not fuel_container_ok(A).

% (2) a pipe, hose, or auxiliary pump
pipe_approved(A):- aircraft(A), pipe_permit(A).
pipe_approved(A):- aircraft(A), pipe_inspection(A).
%-pipe_approved(A):- aircraft(A), not pipe_approved(A).

pipe_ok(A):- aircraft(A), pipe_conform(A).
pipe_ok(A):- aircraft(A), pipe_approved(A).
%-pipe_ok(A):- aircraft(A), not pipe_ok(A).

%-pipe_offense(A):- aircraft(A), pipe_ok(A).
%pipe_offense(A):- aircraft(A), not -pipe_offense(A).
pipe_offense(A):- aircraft(A), not pipe_ok(A).


%%%%%%%%%%%%%% TAKING OFF, LANDING, OR MANEUVERING AIRCRAFT ON HIGHWAYS, ROADS, OR STREETS;  OFFENSE. %%%%%%%%%%%%%%%%%
exception(P):- person(P), prevent_injury(P).
exception(P):- person(P), emergency(P).
exception(P):- person(P), follow_ordinances(P).
%-exception(P):- not exception(P). 



public_road_offense(P):- person(P), take_off_public(P), not exception(P).
public_road_offense(P):- person(P), land_public(P), not exception(P).
public_road_offense(P):- person(P), maneuvers_public(P), not exception(P).

%-public_road_offense(P):- exception(P).
%-public_road_offense(P):- person(P), not public_road_offense(P).



%%%%%%%%%%%% Summarize offenses above %%%%%%%%%%%%%%%%%%%%%

%-person_offense(P, A):- person(P), aircraft(A),
%						not cetificate_offense(P, A),
%						not aircraft_register_offense(A), 
%						not aircraft_ID_offense(A),
%						not fuel_container_offense(A), 
%						not pipe_offense(A),
%						not public_road_offense(P).

%person_offense(P, A):- person(P), aircraft(A), not -person_offense(P, A).


person_offense(air, P, A):- person(P), aircraft(A), cetificate_offense(P, A).
person_offense(air, P, A):- person(P), aircraft(A), aircraft_register_offense(A).
person_offense(air, P, A):- person(P), aircraft(A), aircraft_ID_offense(A).
person_offense(air, P, A):- person(P), aircraft(A), fuel_container_offense(A).
person_offense(air, P, A):- person(P), aircraft(A), pipe_offense(A).
person_offense(air, P, A):- person(P), aircraft(A), public_road_offense(P).



does_not_apply(P, A):- person(P), not aircraft(A).
does_not_apply(P, A):- not person(P), aircraft(A).
does_not_apply(P, A):- not person(P), not aircraft(A).


%result(P, A, Answer):- person(P), aircraft(A), person_offense(P, A).






%%%%%%%%%%%%%%%%%%%%% Test Quries section one && two %%%%%%%%%%%%%%%%%%%%%%
% List information of a person P and the aircraft P navigates
% First, give information. 
%safety_equipment(a1). 
%crew(john).
%in_charge(john).
%person(john).
not_pilot(john).
%has_certificate(john).
%no_certificate(john).
aircraft(a1).
% Then, test.
%?- person_offense(air, john, a1).
%?- not person_offense(john, a1).
%?- aircraft(device).
%?- airman(john).
%?- -aircraft(device).
%?- safety_equipment(device).
%?- result(john, a1, Answer).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% pulishment for first two section %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% SUBCHAPTER C. USE OF PUBLIC ROADS BY AIRCRAFT
% Sec. 24.021.  TAKING OFF, LANDING, OR MANEUVERING AIRCRAFT ON HIGHWAYS, ROADS, OR STREETS;  OFFENSE.  
% (a)  A person commits an offense if the person takes off, lands, or maneuvers an aircraft, whether heavier or lighter than air, on a public highway, road, or street 
%except:
% (1)  when necessary to prevent serious injury to a person or property;
% (2)  during or within a reasonable time after an emergency;  or
% (3)  as provided by Section 24.022.
%
%(b)  An offense under Subsection 
% is a misdemeanor punishable by a fine of not less than $25 and not more than $200.

punish(road, P,W,M):-person_offense(road, P,W),fine(W,M).

fine(takesoffstreets,from25to200).
fine(takesoffhighways,from100to200).
fine(landingstreets,from25to200).
fine(landinghighways,from100to200).

person_offense(road,P,W):-normal_offense(P,W), not except(P,E).

normal_offense(P,W):-person(P), not lawful(W).

lawful(W) :- not _lawful(W).
_lawful(W) :- operate(W).

operate(takesoffstreets).
operate(takesoffhighways).
operate(landingstreets).
operate(landinghighways).

except(P,E):- exception(P,injury).
except(P,E):- exception(P,emergency).
except(P,E):- exception(P,inspecficsection).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   the fact part    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
person(tom).

% delete the following exception(tom,injury) or not will affects the answer set
% if deleted, means no exception, offense
% if not, means with an exception, not offense, return the answer set
% exception(tom,injury). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    the query part  section three %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checked 
%?-normal_offense(tom,takesoffhighways).   
% checked
%?-person_offense(road, tom,takesoffhighways).
% checked
?- punish(road,tom,takesoffhighways,M).
% unchecked
% ?- punish(air,john,a1,M).






