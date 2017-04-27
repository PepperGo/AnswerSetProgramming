%%%%%%%%%%%%%%%%% Definitions %%%%%%%%%%%%%%%%%%%%%%%%
% (1) "Aircraft" means a device that is invented, used, or designated for air navigation or flight, 
% other than a parachute or other device used primarily as safety equipment.

% Aircraft(A) is true if a aircraft A satisfies the aircraf definiton.

aircraft(A):- air_navigation(A), 
			  not safety_equipment(A).

aircraft(A):- flight(A), 
			  not safety_equipment(A).		 

% (2)  "Airman" means:
% (A)  a person, including the person in command of an aircraft or a pilot, mechanic, or member of the crew, 
%      who engages in the navigation of an aircraft while under way;  or
% (B)  the person who is in charge of the inspection, overhaul, or repair of an aircraft.

% airman(P) is true if a person(P) satisfies the airman defition.

person(P).
airman(P):- person(P), pilot(P).
airman(P):- person(P), mechanic(P).
airman(P):- person(P), crew(P).
airman(P):- person(P), in_charge(P).

% (3)  "Airman certificate" means a certificate issued to an airman under 49 U.S.C. Section 1422.
% airman_certificate(P) is true if P is an airman, and P has a certificate.

-has_certificate(P):- no_certificate(P).

airman_certificate(P):- airman(P), has_certificate(P).
-airman_certificate(P):- not airman_certificate(P).




%%%%%%%%%%%%%%%%%%% Offense %%%%%%%%%%%%%%%%%%%%%%%%%
person_offense(P):- person(P),
					-airman_certificate(P).


% First, give information. 
%crew(john).
%in_charge(john).
% pilot(john).
% no_certificate(john).
% Then, test.
% ?- airman(john).



%%%%%%%%%%%%%%%%% 
% SUBCHAPTER C. USE OF PUBLIC ROADS BY AIRCRAFT
% Sec. 24.021.  TAKING OFF, LANDING, OR MANEUVERING AIRCRAFT ON HIGHWAYS, ROADS, OR STREETS;  OFFENSE.  
% (a)  A person commits an offense if the person takes off, lands, or maneuvers an aircraft, whether heavier or lighter than air, on a public highway, road, or street 
%except:
% (1)  when necessary to prevent serious injury to a person or property;
% (2)  during or within a reasonable time after an emergency;  or
% (3)  as provided by Section 24.022.
person_offense(P):-person(P), not lawful(P), not except(P).
lawful(W) :- not _lawful(W).
_lawful(W) :- invaild(W).
invalid(W) :- takesoffstreets(W).
invaild(W) :- takesoffhighways(W).
invalid(W) :- landingstreets(W).
invaild(W) :- landinghighways(W).
except(P):- injury(P).
except(P):- emergency(P).
%except(P):- inspecficsection(P).

person(tom).
takesoffhighways(tom).
injury(tom).
?-person_offense(tom).




