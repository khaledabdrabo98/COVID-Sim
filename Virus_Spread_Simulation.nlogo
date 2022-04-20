;;;;;;;;;;;;;;;;;;
;; Declarations ;;
;;;;;;;;;;;;;;;;;;

globals
[
  ;; number of humans in the population
  total_population
  ;; number of humans that are healthy
  num_healthy
  ;; number of humans that are sick
  num_infected
  ;; number of humans that are dead
  num_dead
  ;; mask penetration rate (mask infection rate)
  mask_penetration_rate
  ;; when multiple runs are recorded in the plot, this
  ;; tracks what run number we're on
  run-number
  ;; timer when infection start
  timerInf
  ;; counter used to keep the model running for a little
  ;; while after the last turtle gets infected
  delay
  pop_infected_daily
]

breed [humans human]
breed [dead deads]

humans-own [
  infected?
  contagious?
  infected_previously?
  wearmask?
  infection-duration
  symptom_delay_duration
  isolate_symptomatic_individuals?
  feel_symptoms?
  isolation_tracker?
  current_infection_hours
]

;;;;;;;;;;;;;;;;;;;;;
;; Setup Functions ;;
;;;;;;;;;;;;;;;;;;;;;

to setup-agents [#total-humans]
  create-humans #total-humans [
    set infected? false
    set contagious? false
    set infected_previously? false
    set wearmask? false
    set isolate_symptomatic_individuals? false
    set feel_symptoms? false
    set isolation_tracker? false
    setxy random-xcor random-ycor
  ]

  ask humans [
    set shape "person"
    set size 2
    set color green
  ]

end

to setup-globals
  ;; random-seed random_seed_number
  set num_infected 0
  set num_dead 0
  set total_population population
  set mask_penetration_rate (mask_penetration_particles / 100)
  set pop_infected_daily []
end

to setup
  clear-all
  setup-globals
  setup-agents total_population
  set total_population (count humans + count dead)
  let initial_infected_humans round (count humans * (initial_population_infected / 100)) ;true number of initial infected humans
  infect_people initial_infected_humans ;start off by having some infected people

  let initial_wear_mask round (count humans * (use_mask / 100))
  wear_mask initial_wear_mask

  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

to infect_people [#initial_infected_humans]
  ask n-of #initial_infected_humans humans with [not infected_previously?] [get-infected]
end

to wear_mask [#initial_wear_mask]
  ask n-of #initial_wear_mask humans [set wearmask? true]
  ask humans with [wearmask?] [set shape "person_mask"]
end

to go
  ask humans [move-forward-randomly]
  ask humans [infect]
  ask humans [infection_aftermath]

  set pop_infected_daily lput (100 * count humans with [infected?] / total_population) pop_infected_daily

  if num_infected = ((count humans with [not infected? and infected_previously?]) + num_dead) [
    stop
  ]
    tick
end


to move-forward-randomly
  let lockdown_delay_hours lockdown_delay * 24

  ifelse (not feel_symptoms?) [ ;people only move if no symptoms
    ifelse Total_lockdown? [ ;if no symptoms and total lockdown, don't move, except for the ones disobeying and essential workers
      ifelse coin-flip? [right random 45] [left random 45]
    ]
    [ ;if there is no total lockdown...
        ifelse coin-flip? [right random 180] [left random 180]
          forward random-float 0.2
    ]
  ]
  [ ;if they feel symptoms, stop moving, i.e. no command.
  ]
end


to infect
  if (not infected_previously?) [ ;only applies to people not infected
    let people_around humans-on neighbors ;let "people_around" be the neighbors
    ifelse (not wearmask?) ;referring to the person ITSELF
    [let infectious_around people_around with [(infected? = true and contagious? = true)] ;people are infectious if they are infected + contagious
     let infectious_around_mask infectious_around with [wearmask?]
     let infectious_around_nomask infectious_around with [not wearmask?]
     let number_of_infectious_around count infectious_around
     let number_of_infectious_around_nomask count infectious_around_nomask
     let number_of_infectious_around_mask count infectious_around_mask

     if number_of_infectious_around > 0 [ ;if there are infected no-mask people around
       let within_infectious_distance (random(metres_per_patch) + 1) ;define infectious distance
       set within_infectious_distance within_infectious_distance + random-float ( social_distancing )
       ifelse (not wearmask?) [ ;referring to neighbours without masks
         if (infection-chance >= (random(100) + 1)) and within_infectious_distance <= maximum_infectious_distance [
           get-infected
         ]
       ]
       [
         if (mask_penetration_rate * infection-chance >= (random(100) + 1)) and within_infectious_distance <= maximum_infectious_distance [  ;infected according to infection-chance + within distance
           get-infected
         ]
       ]
      ]
    ]

    [let infectious_around people_around with [(infected? = true and contagious? = true)]
     let infectious_around_mask infectious_around with [wearmask?]
     let infectious_around_nomask infectious_around with [not wearmask?]
     let number_of_infectious_around count infectious_around
     let number_of_infectious_around_nomask count infectious_around_nomask
     let number_of_infectious_around_mask count infectious_around_mask
     if number_of_infectious_around > 0 [ ;if there are infected people around
       let within_infectious_distance (random(metres_per_patch) + 1) ;define infectious distance
       set within_infectious_distance within_infectious_distance + random-float ( social_distancing )
       ifelse (not wearmask?) [
          if ((mask_penetration_rate) * infection-chance >= (random(100) + 1)) and within_infectious_distance <= maximum_infectious_distance [   ;same principle as above but we multiply by penetration rate because victim is already wearing mask
            get-infected
          ]
       ]
       [
          if ((mask_penetration_rate) * (mask_penetration_rate) * infection-chance >= (random(100) + 1)) and within_infectious_distance <= maximum_infectious_distance [   ;infected according to infection-chance + within distance
            get-infected
          ] ;we multiply by mask penetration rate twice because victim and infector is wearing mask; therefore two layers of masks
       ]
     ]
    ]
  ]
end


to get-infected
  set infected_previously? true
  set infected? true
  set contagious? true
  set color yellow
  set infection-duration 24 * (random-normal infection_average_duration 2) ;avg hours of infection
  set symptom_delay_duration 24 * (random-normal days_before_symptoms 1) ;duration (converted to ticks or hours) before symptoms show
  set current_infection_hours 0
  set num_infected num_infected + 1
end

to recover
  set infected? false
  set feel_symptoms? false
  set contagious? false
  set infection-duration 0
  set current_infection_hours 0
  set color blue
  set shape "person"
  set size 2

end


to infection_aftermath
  if infected? [
    if (current_infection_hours >= symptom_delay_duration) [
      set feel_symptoms? true

      if feel_symptoms? [
        set color 14
        set shape "person"
      ]

      if feel_symptoms? [
        ;Put slider bar here for isolation
        if (symptomatic_isolation_rate >= (random(100) + 1)) and (not isolation_tracker?) [
        set isolate_symptomatic_individuals? true
        ]
      set isolation_tracker? true ;isolation_tracker is just a variable to make sure this coin-flip is only applied once (and no more) to each symptomatic individual
      ]

    ]

    ifelse (current_infection_hours >= infection-duration)
    [

      ifelse infected? [ ;if severe symptoms...
        ifelse (fatality_rate >= (random (100) + 1)) [ ;...flip a coin, if dead....
          set num_dead num_dead + 1
          set breed dead
          set shape "x"
          set size 1
          set color red
        ]
        [ ;if not dead...recover
          set current_infection_hours 0
          recover
        ]
      ]

      [ ;if light symptoms, recover at end of period
      set current_infection_hours 0
      recover
      ]
    ]
    [
      set current_infection_hours current_infection_hours + 1
    ]
  ]

end


to-report coin-flip?
  report random 2 = 0 ;reports outcome of 0 or 1, if 0 then its true
end
@#$#@#$#@
GRAPHICS-WINDOW
262
16
1035
790
-1
-1
15.0
1
10
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
10.0

BUTTON
1053
84
1208
117
Begin simulation
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
22
200
229
233
infection-chance
infection-chance
0
100
89.0
1
1
%
HORIZONTAL

SLIDER
21
40
228
73
population
population
1
1000
500.0
1
1
NIL
HORIZONTAL

BUTTON
1055
125
1209
158
Infect population
infect_people round (count humans * (initial_population_infected / 100))\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
1053
44
1208
77
Setup simulation
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
89
227
122
initial_population_infected
initial_population_infected
0
10
5.0
1
1
%
HORIZONTAL

TEXTBOX
21
181
171
199
Virus parameters
12
0.0
1

TEXTBOX
25
16
175
34
Population parameters\n
12
0.0
1

TEXTBOX
1055
22
1205
40
Control center
12
0.0
1

SLIDER
21
239
230
272
infection_average_duration
infection_average_duration
0
50
21.0
1
1
days
HORIZONTAL

SLIDER
20
278
231
311
maximum_infectious_distance
maximum_infectious_distance
0
5
2.0
0.5
1
metres
HORIZONTAL

SLIDER
19
316
232
349
days_before_symptoms
days_before_symptoms
0
21
6.0
1
1
days
HORIZONTAL

TEXTBOX
24
370
174
388
Masks impact\n
12
0.0
1

SLIDER
23
390
230
423
use_mask
use_mask
0
100
45.0
1
1
%
HORIZONTAL

SLIDER
23
429
231
462
mask_penetration_particles
mask_penetration_particles
0
100
39.0
1
1
%
HORIZONTAL

TEXTBOX
21
479
171
497
Confinement
12
0.0
1

SLIDER
21
539
227
572
lockdown_delay
lockdown_delay
0
100
0.0
1
1
days
HORIZONTAL

SWITCH
22
501
228
534
total_lockdown?
total_lockdown?
1
1
-1000

SLIDER
17
610
228
643
social_distancing
social_distancing
0
4
0.0
0.5
1
metres
HORIZONTAL

TEXTBOX
23
589
226
619
Effect of social distancing
12
0.0
1

PLOT
1053
580
1755
790
Infectivity
Time (hours)
% of population
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Infected" 1.0 0 -1604481 true "" "plot 100 * count humans with [infected?] / total_population"
"Infected no symproms" 1.0 0 -1184463 true "" "plot 100 * count humans with [infected? and not feel_symptoms?] / total_population"
"Infected with symptoms" 1.0 0 -5298144 true "" "plot 100 * count humans with [infected? and feel_symptoms?] / total_population"
"Dead" 1.0 0 -16777216 true "" "plot 100 * num_dead / total_population"

PLOT
1053
368
1541
571
SIRD model
Time (hours)
% population
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Susceptible" 1.0 0 -13840069 true "" "plot 100 * (count humans with [not infected? and not infected_previously?]) / total_population"
"Infected" 1.0 0 -2674135 true "" "plot 100 * (count humans with [color = yellow] + count humans with [color = orange]) / total_population"
"Recovered" 1.0 0 -13345367 true "" "plot 100 * (count humans with [color = blue]) / total_population"
"Dead" 1.0 0 -16777216 true "" "plot 100 * (num_dead) / total_population"

MONITOR
1054
196
1190
241
% currently infected
precision (100 * count humans with [infected?] / (count humans)) 0
17
1
11

TEXTBOX
1055
175
1205
193
Stats
12
0.0
1

MONITOR
1053
254
1190
299
% asymptomatic
precision (100 * count humans with [infected? and not feel_symptoms?] / (count humans with [infected?])) 0
17
1
11

MONITOR
1353
131
1600
176
Days elapsed since simulation started
precision (ticks / 24) 1
17
1
11

TEXTBOX
24
659
174
677
Environment scaling\n
12
0.0
1

SLIDER
18
679
230
712
metres_per_patch
metres_per_patch
0
40
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
22
727
172
745
Effect of isolation
12
0.0
1

SLIDER
20
745
227
778
symptomatic_isolation_rate
symptomatic_isolation_rate
0
100
28.0
1
1
%
HORIZONTAL

MONITOR
1213
308
1326
353
Total death
num_dead
17
1
11

MONITOR
1213
197
1326
242
Total population
population
17
1
11

MONITOR
1211
249
1324
294
Total infected
num_infected
17
1
11

MONITOR
1345
198
1461
243
Total recovered
count humans with [not infected? and infected_previously?]
17
1
11

TEXTBOX
22
796
172
814
Fatality rate
12
0.0
1

SLIDER
21
818
230
851
fatality_rate
fatality_rate
0
100
30.0
1
1
%
HORIZONTAL

MONITOR
1344
249
1476
294
Total survivers
count humans with [not infected? and not infected_previously?]
17
1
11

@#$#@#$#@
## WHAT IS IT?

Disease Solo is a one-player version of the HubNet activity Disease.  It simulates the spread of a disease through a population.  One agent in the population is a person controlled by the user; the others are "androids" controlled by the computer.

## HOW IT WORKS

The user controls the blue agent via the buttons and slider on the right side of the view.  The infection is started by pressing the "infect" button.

Sick agents are indicated by a red circle.

Androids can move using a few different simple strategies. By default they simply move randomly, however, using the AVOID? and CHASE? switches you can indicate that uninfected androids should run from infected ones or infected androids should chase uninfected ones.

The person may also catch the infection.

Healthy "agents" on the same patch as sick agents have an INFECTION-CHANCE chance of becoming ill.

## HOW TO USE IT

### Buttons

SETUP/CLEAR - sets up the world and clears plots.
SETUP/KEEP - sets up the world without clearing the plot; this lets you compare results from different runs.
GO - runs the simulation.
INFECT - infects one of the androids

### Sliders

NUM-ANDROIDS - determines how many androids are created at setup
INFECTION-CHANCE - a healthy agent's chance at every time step to become sick if it is on the same patch as an infected agent

### Monitors

NUMBER SICK - the number of sick agents

### Plots

NUMBER SICK - the number of sick agents versus time

### Switches

AVOID? - when this switch is on each uninfected android checks all four directions to see if it can move to a patch that is safe from infected agents.
CHASE? - when this switch is on each infected androids checks all four directions to see if it can infect another agent.

### User controls

UP, DOWN, LEFT, and RIGHT - move the person around the world, STEP-SIZE determines how far the person moves each time one of the control buttons is pressed.

## THINGS TO NOTICE

Think about how the plot will change if you alter a parameter.  Altering the infection chance will have different effects on the plot.

## THINGS TO TRY

Do several runs of the model and record a data set for each one by using the setup/keep button. Compare the different resulting plots.

What happens to the plot as you do runs with more and more androids?

## EXTENDING THE MODEL

Currently, the agents remain sick once they're infected.  How would the shape of the plot change if agents eventually healed?  If, after healing, they were immune to the disease, or could still spread the disease, how would the dynamics be altered?

The user has a distinct advantage in this version of the model (assuming that the goal is either not to become infected, or to infect others), as the user can see the entire world and the androids can only see one patch ahead of them.  Try to even out the playing field by giving the androids a larger field of vision.

Determining the first agent who is infected may impact the way disease spreads through the population.  Try changing the target of the first infection so it can be determined by the user.

## NETLOGO FEATURES

You can use the keyboard to control the person.  To activate the keyboard shortcuts for the movement button, either hide the command center or click in the white background.

The plot uses temporary plot pens, rather than a fixed set of permanent plot pens, so you can use the setup/keep button to overlay as many runs as you want.

## RELATED MODELS

* Disease (HubNet version)
* Virus
* HIV

## CREDITS AND REFERENCES

This model is a one player version of the HubNet activity Disease.  In the HubNet version, multiple users can participate at once.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (2005).  NetLogo Disease Solo model.  http://ccl.northwestern.edu/netlogo/models/DiseaseSolo.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2005 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2005 -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

airplane sick
false
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15
Circle -2674135 true false 156 156 108

android
false
0
Polygon -7500403 true true 210 90 240 195 210 210 165 90
Circle -7500403 true true 110 3 80
Polygon -7500403 true true 105 88 120 193 105 240 105 298 135 300 150 210 165 300 195 298 195 240 180 193 195 88
Rectangle -7500403 true true 127 81 172 96
Rectangle -16777216 true false 135 33 165 60
Polygon -7500403 true true 90 90 60 195 90 210 135 90

android sick
false
0
Polygon -7500403 true true 210 90 240 195 210 210 165 90
Circle -7500403 true true 110 3 80
Polygon -7500403 true true 105 88 120 193 105 240 105 298 135 300 150 210 165 300 195 298 195 240 180 193 195 88
Rectangle -7500403 true true 127 81 172 96
Rectangle -16777216 true false 135 33 165 60
Polygon -7500403 true true 90 90 60 195 90 210 135 90
Circle -2674135 true false 150 120 120

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

box sick
false
0
Polygon -7500403 true true 150 285 270 225 270 90 150 150
Polygon -7500403 true true 150 150 30 90 150 30 270 90
Polygon -7500403 true true 30 90 30 225 150 285 150 150
Line -16777216 false 150 285 150 150
Line -16777216 false 150 150 30 90
Line -16777216 false 150 150 270 90
Circle -2674135 true false 170 178 108

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

butterfly sick
false
0
Rectangle -7500403 true true 92 135 207 224
Circle -7500403 true true 158 53 134
Circle -7500403 true true 165 180 90
Circle -7500403 true true 45 180 90
Circle -7500403 true true 8 53 134
Line -16777216 false 43 189 253 189
Rectangle -7500403 true true 135 60 165 285
Circle -7500403 true true 165 15 30
Circle -7500403 true true 105 15 30
Line -7500403 true 120 30 135 60
Line -7500403 true 165 60 180 30
Line -16777216 false 135 60 135 285
Line -16777216 false 165 285 165 60
Circle -2674135 true false 156 171 108

cactus
false
0
Rectangle -7500403 true true 135 30 175 177
Rectangle -7500403 true true 67 105 100 214
Rectangle -7500403 true true 217 89 251 167
Rectangle -7500403 true true 157 151 220 185
Rectangle -7500403 true true 94 189 148 233
Rectangle -7500403 true true 135 162 184 297
Circle -7500403 true true 219 76 28
Circle -7500403 true true 138 7 34
Circle -7500403 true true 67 93 30
Circle -7500403 true true 201 145 40
Circle -7500403 true true 69 193 40

cactus sick
false
0
Rectangle -7500403 true true 135 30 175 177
Rectangle -7500403 true true 67 105 100 214
Rectangle -7500403 true true 217 89 251 167
Rectangle -7500403 true true 157 151 220 185
Rectangle -7500403 true true 94 189 148 233
Rectangle -7500403 true true 135 162 184 297
Circle -7500403 true true 219 76 28
Circle -7500403 true true 138 7 34
Circle -7500403 true true 67 93 30
Circle -7500403 true true 201 145 40
Circle -7500403 true true 69 193 40
Circle -2674135 true false 156 171 108

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

car sick
false
0
Polygon -7500403 true true 285 208 285 178 279 164 261 144 240 135 226 132 213 106 199 84 171 68 149 68 129 68 75 75 15 150 15 165 15 225 285 225 283 174 283 176
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 195 90 135 90 135 135 210 135 195 105 165 90
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58
Circle -2674135 true false 171 156 108

cat
false
0
Line -7500403 true 285 240 210 240
Line -7500403 true 195 300 165 255
Line -7500403 true 15 240 90 240
Line -7500403 true 285 285 195 240
Line -7500403 true 105 300 135 255
Line -16777216 false 150 270 150 285
Line -16777216 false 15 75 15 120
Polygon -7500403 true true 300 15 285 30 255 30 225 75 195 60 255 15
Polygon -7500403 true true 285 135 210 135 180 150 180 45 285 90
Polygon -7500403 true true 120 45 120 210 180 210 180 45
Polygon -7500403 true true 180 195 165 300 240 285 255 225 285 195
Polygon -7500403 true true 180 225 195 285 165 300 150 300 150 255 165 225
Polygon -7500403 true true 195 195 195 165 225 150 255 135 285 135 285 195
Polygon -7500403 true true 15 135 90 135 120 150 120 45 15 90
Polygon -7500403 true true 120 195 135 300 60 285 45 225 15 195
Polygon -7500403 true true 120 225 105 285 135 300 150 300 150 255 135 225
Polygon -7500403 true true 105 195 105 165 75 150 45 135 15 135 15 195
Polygon -7500403 true true 285 120 270 90 285 15 300 15
Line -7500403 true 15 285 105 240
Polygon -7500403 true true 15 120 30 90 15 15 0 15
Polygon -7500403 true true 0 15 15 30 45 30 75 75 105 60 45 15
Line -16777216 false 164 262 209 262
Line -16777216 false 223 231 208 261
Line -16777216 false 136 262 91 262
Line -16777216 false 77 231 92 261

cat sick
false
0
Line -7500403 true 285 240 210 240
Line -7500403 true 195 300 165 255
Line -7500403 true 15 240 90 240
Line -7500403 true 285 285 195 240
Line -7500403 true 105 300 135 255
Line -16777216 false 150 270 150 285
Line -16777216 false 15 75 15 120
Polygon -7500403 true true 300 15 285 30 255 30 225 75 195 60 255 15
Polygon -7500403 true true 285 135 210 135 180 150 180 45 285 90
Polygon -7500403 true true 120 45 120 210 180 210 180 45
Polygon -7500403 true true 180 195 165 300 240 285 255 225 285 195
Polygon -7500403 true true 180 225 195 285 165 300 150 300 150 255 165 225
Polygon -7500403 true true 195 195 195 165 225 150 255 135 285 135 285 195
Polygon -7500403 true true 15 135 90 135 120 150 120 45 15 90
Polygon -7500403 true true 120 195 135 300 60 285 45 225 15 195
Polygon -7500403 true true 120 225 105 285 135 300 150 300 150 255 135 225
Polygon -7500403 true true 105 195 105 165 75 150 45 135 15 135 15 195
Polygon -7500403 true true 285 120 270 90 285 15 300 15
Line -7500403 true 15 285 105 240
Polygon -7500403 true true 15 120 30 90 15 15 0 15
Polygon -7500403 true true 0 15 15 30 45 30 75 75 105 60 45 15
Line -16777216 false 164 262 209 262
Line -16777216 false 223 231 208 261
Line -16777216 false 136 262 91 262
Line -16777216 false 77 231 92 261
Circle -2674135 true false 186 186 108

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cow skull
false
0
Polygon -7500403 true true 150 90 75 105 60 150 75 210 105 285 195 285 225 210 240 150 225 105
Polygon -16777216 true false 150 150 90 195 90 150
Polygon -16777216 true false 150 150 210 195 210 150
Polygon -16777216 true false 105 285 135 270 150 285 165 270 195 285
Polygon -7500403 true true 240 150 263 143 278 126 287 102 287 79 280 53 273 38 261 25 246 15 227 8 241 26 253 46 258 68 257 96 246 116 229 126
Polygon -7500403 true true 60 150 37 143 22 126 13 102 13 79 20 53 27 38 39 25 54 15 73 8 59 26 47 46 42 68 43 96 54 116 71 126

cow skull sick
false
0
Polygon -7500403 true true 150 90 75 105 60 150 75 210 105 285 195 285 225 210 240 150 225 105
Polygon -16777216 true false 150 150 90 195 90 150
Polygon -16777216 true false 150 150 210 195 210 150
Polygon -16777216 true false 105 285 135 270 150 285 165 270 195 285
Polygon -7500403 true true 240 150 263 143 278 126 287 102 287 79 280 53 273 38 261 25 246 15 227 8 241 26 253 46 258 68 257 96 246 116 229 126
Polygon -7500403 true true 60 150 37 143 22 126 13 102 13 79 20 53 27 38 39 25 54 15 73 8 59 26 47 46 42 68 43 96 54 116 71 126
Circle -2674135 true false 156 186 108

cylinder
false
0
Circle -7500403 true true 0 0 300

dog
false
0
Polygon -7500403 true true 300 165 300 195 270 210 183 204 180 240 165 270 165 300 120 300 0 240 45 165 75 90 75 45 105 15 135 45 165 45 180 15 225 15 255 30 225 30 210 60 225 90 225 105
Polygon -16777216 true false 0 240 120 300 165 300 165 285 120 285 10 221
Line -16777216 false 210 60 180 45
Line -16777216 false 90 45 90 90
Line -16777216 false 90 90 105 105
Line -16777216 false 105 105 135 60
Line -16777216 false 90 45 135 60
Line -16777216 false 135 60 135 45
Line -16777216 false 181 203 151 203
Line -16777216 false 150 201 105 171
Circle -16777216 true false 171 88 34
Circle -16777216 false false 261 162 30

dog sick
false
0
Polygon -7500403 true true 300 165 300 195 270 210 183 204 180 240 165 270 165 300 120 300 0 240 45 165 75 90 75 45 105 15 135 45 165 45 180 15 225 15 255 30 225 30 210 60 225 90 225 105
Polygon -16777216 true false 0 240 120 300 165 300 165 285 120 285 10 221
Line -16777216 false 210 60 180 45
Line -16777216 false 90 45 90 90
Line -16777216 false 90 90 105 105
Line -16777216 false 105 105 135 60
Line -16777216 false 90 45 135 60
Line -16777216 false 135 60 135 45
Line -16777216 false 181 203 151 203
Line -16777216 false 150 201 105 171
Circle -16777216 true false 171 88 34
Circle -16777216 false false 261 162 30
Circle -2674135 true false 126 186 108

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

ghost
false
0
Polygon -7500403 true true 30 165 13 164 -2 149 0 135 -2 119 0 105 15 75 30 75 58 104 43 119 43 134 58 134 73 134 88 104 73 44 78 14 103 -1 193 -1 223 29 208 89 208 119 238 134 253 119 240 105 238 89 240 75 255 60 270 60 283 74 300 90 298 104 298 119 300 135 285 135 285 150 268 164 238 179 208 164 208 194 238 209 253 224 268 239 268 269 238 299 178 299 148 284 103 269 58 284 43 299 58 269 103 254 148 254 193 254 163 239 118 209 88 179 73 179 58 164
Line -16777216 false 189 253 215 253
Circle -16777216 true false 102 30 30
Polygon -16777216 true false 165 105 135 105 120 120 105 105 135 75 165 75 195 105 180 120
Circle -16777216 true false 160 30 30

ghost sick
false
0
Polygon -7500403 true true 30 165 13 164 -2 149 0 135 -2 119 0 105 15 75 30 75 58 104 43 119 43 134 58 134 73 134 88 104 73 44 78 14 103 -1 193 -1 223 29 208 89 208 119 238 134 253 119 240 105 238 89 240 75 255 60 270 60 283 74 300 90 298 104 298 119 300 135 285 135 285 150 268 164 238 179 208 164 208 194 238 209 253 224 268 239 268 269 238 299 178 299 148 284 103 269 58 284 43 299 58 269 103 254 148 254 193 254 163 239 118 209 88 179 73 179 58 164
Line -16777216 false 189 253 215 253
Circle -16777216 true false 102 30 30
Polygon -16777216 true false 165 105 135 105 120 120 105 105 135 75 165 75 195 105 180 120
Circle -16777216 true false 160 30 30
Circle -2674135 true false 156 171 108

heart
false
0
Circle -7500403 true true 152 19 134
Polygon -7500403 true true 150 105 240 105 270 135 150 270
Polygon -7500403 true true 150 105 60 105 30 135 150 270
Line -7500403 true 150 270 150 135
Rectangle -7500403 true true 135 90 180 135
Circle -7500403 true true 14 19 134

heart sick
false
0
Circle -7500403 true true 152 19 134
Polygon -7500403 true true 150 105 240 105 270 135 150 270
Polygon -7500403 true true 150 105 60 105 30 135 150 270
Line -7500403 true 150 270 150 135
Rectangle -7500403 true true 135 90 180 135
Circle -7500403 true true 14 19 134
Circle -2674135 true false 171 156 108

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

key
false
0
Rectangle -7500403 true true 90 120 300 150
Rectangle -7500403 true true 270 135 300 195
Rectangle -7500403 true true 195 135 225 195
Circle -7500403 true true 0 60 150
Circle -16777216 true false 30 90 90

key sick
false
0
Rectangle -7500403 true true 90 120 300 150
Rectangle -7500403 true true 270 135 300 195
Rectangle -7500403 true true 195 135 225 195
Circle -7500403 true true 0 60 150
Circle -16777216 true false 30 90 90
Circle -2674135 true false 156 171 108

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

leaf sick
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195
Circle -2674135 true false 141 171 108

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

monster
false
0
Polygon -7500403 true true 75 150 90 195 210 195 225 150 255 120 255 45 180 0 120 0 45 45 45 120
Circle -16777216 true false 165 60 60
Circle -16777216 true false 75 60 60
Polygon -7500403 true true 225 150 285 195 285 285 255 300 255 210 180 165
Polygon -7500403 true true 75 150 15 195 15 285 45 300 45 210 120 165
Polygon -7500403 true true 210 210 225 285 195 285 165 165
Polygon -7500403 true true 90 210 75 285 105 285 135 165
Rectangle -7500403 true true 135 165 165 270

monster sick
false
0
Polygon -7500403 true true 75 150 90 195 210 195 225 150 255 120 255 45 180 0 120 0 45 45 45 120
Circle -16777216 true false 165 60 60
Circle -16777216 true false 75 60 60
Polygon -7500403 true true 225 150 285 195 285 285 255 300 255 210 180 165
Polygon -7500403 true true 75 150 15 195 15 285 45 300 45 210 120 165
Polygon -7500403 true true 210 210 225 285 195 285 165 165
Polygon -7500403 true true 90 210 75 285 105 285 135 165
Rectangle -7500403 true true 135 165 165 270
Circle -2674135 true false 141 141 108

moon
false
0
Polygon -7500403 true true 175 7 83 36 25 108 27 186 79 250 134 271 205 274 281 239 207 233 152 216 113 185 104 132 110 77 132 51

moon sick
false
0
Polygon -7500403 true true 160 7 68 36 10 108 12 186 64 250 119 271 190 274 266 239 192 233 137 216 98 185 89 132 95 77 117 51
Circle -2674135 true false 171 171 108

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person sick
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Circle -2674135 true false 178 163 95

person_mask
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Polygon -13345367 true false 105 45 120 75 180 75 195 45 105 45 120 75
Line -1 false 120 75 180 75
Line -1 false 105 45 195 45
Line -1 false 120 75 105 45
Line -1 false 180 75 195 45
Line -1 false 135 60 165 60

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

star sick
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108
Circle -2674135 true false 156 171 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

target sick
true
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60
Circle -2674135 true false 163 163 95

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wheel sick
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269
Circle -2674135 true false 156 156 108

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
random-seed 3
setup-clear
infect
repeat 100 [ go ]
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
