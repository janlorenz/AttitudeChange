extensions [r]

turtles-own [opinion group initial_opinion opinion-list focus?]
globals [draw? cum_fragmentation cum_sd cum_biasmean cum_extremity cum_extremists]

;; BUTTON PROCEDURES

to setup
  r:eval "source('/home/janlo/Documents/od/SocPsy/fragmentation.R')"
  clear-all
  ask patches [set pcolor white]
  create-turtles N
  reset-ticks
  ask turtles [
    set group random 2 ;; group is either 0 or 1
    set opinion new_opinion
    set initial_opinion opinion
    set opinion-list (list opinion)
    setxy 0 confine-opinion-scale-to-max-pycor opinion M
    set color 65 + group * 60
    set focus? false
    ]
  ask one-of turtles [set focus? true]
  set draw? true
  update-plots
end

to go
  if (count turtles = 0) [setup]
  repeat skip_ticks_draw [
    repeat iterations_per_tick [ask turtles [ update_opinion ]]
    ask turtles [ set opinion-list lput opinion opinion-list ] ;; update the opinion-list
    ask turtles [ if (length opinion-list = max-pxcor + 1) [ set opinion-list butfirst opinion-list ] ] ;; cut oldest values for "rolling" opinion list
    tick
    if (ticks = spinup_tick) [
      set cum_fragmentation fragmentation
      set cum_sd standard-deviation [opinion] of turtles
      set cum_biasmean abs mean [opinion] of turtles
      set cum_extremity extremity
    ]
    if (ticks > spinup_tick) [
      set cum_fragmentation ((ticks - spinup_tick) * cum_fragmentation + fragmentation) / (ticks - spinup_tick + 1)
      set cum_sd ((ticks - spinup_tick) * cum_sd + standard-deviation [opinion] of turtles) /  (ticks - spinup_tick + 1)
      set cum_biasmean ((ticks - spinup_tick) * cum_biasmean + abs mean [opinion] of turtles) /  (ticks - spinup_tick + 1)
      set cum_extremity ((ticks - spinup_tick) * cum_extremity + extremity) /  (ticks - spinup_tick + 1)
      set cum_extremists ((ticks - spinup_tick) * cum_extremists + fraction_extremists) /  (ticks - spinup_tick + 1)
    ]
  ]
  if (draw?) [draw_trajectories]
end

to change_focus
  ask turtles [set focus? false]
  ask one-of turtles [set focus? true]
end

to run_until_world_full
  setup
  set draw? false
  let temp skip_ticks_draw
  set skip_ticks_draw 1
  repeat max-pxcor [go]
  draw_trajectories
  set skip_ticks_draw temp
end

;; INTERNAL PROCEDURES

to update_opinion
  ifelse (random-float 1 < theta and theta_as != "weight on initial attitude") [
    if (theta_as = "probability for new random") [set opinion new_opinion]
    if (theta_as = "probability for initial attitude") [set opinion initial_opinion]
  ][
    let other_turtle one-of turtles
    let source_credibility ifelse-value (group = [group] of other_turtle) [1] [intergroup_credibility]
    ifelse (theta_as = "weight on initial attitude") [
      set opinion (1 - theta) * (opinion + opinion_change opinion source_credibility [opinion] of other_turtle) + theta * initial_opinion
    ][
      set opinion opinion + opinion_change opinion source_credibility [opinion] of other_turtle
    ]
   ]
end

to draw_trajectories
  ;; let turtles move with their opinion trajectories from left to right across the world drawing trajectories or coloring patches
  clear-drawing
  ask turtles [
    pen-up
    setxy 0 (confine-opinion-scale-to-max-pycor (item 0 opinion-list) M)
    if (visualization = "Agents' trajectories") [
      set color ifelse-value (intergroup_credibility = 1 and initial_groupspread = 0) [item (who mod length base-colors) base-colors] [65 + group * 60]
    ]
  ]
  let t-counter 1
  while [ t-counter < (length ( [opinion-list] of turtle 1 )) ] [
    ask turtles [ pen-up ]
    if (visualization = "Agents' trajectories") [ ask turtles [ pen-down ] ]
    foreach sort turtles [ [?1] -> ;; with foreach for drawing always in the same order
       ask ?1 [setxy t-counter (confine-opinion-scale-to-max-pycor (item t-counter opinion-list) M) ]
    ]
    ifelse (visualization = "Heatmap timeline")
      [ ask patches with [pxcor = t-counter ] [ set pcolor colorcode (count turtles-here / count turtles) color_axis_max ] ]
      [ ask patches [ set pcolor white ] ]
    set t-counter t-counter + 1
  ]
  if (focus_one) [
    ask turtles with [focus?] [
      pen-up
      setxy 0 (confine-opinion-scale-to-max-pycor (item 0 opinion-list) M)
      pen-down
      set pen-size 3
      set color white
      set t-counter 1
      while [ t-counter < (length ( [opinion-list] of turtle 1 )) ] [
        setxy t-counter (confine-opinion-scale-to-max-pycor (item t-counter opinion-list) M)
        set t-counter t-counter + 1
      ]
      pen-up
      setxy 0 (confine-opinion-scale-to-max-pycor (item 0 opinion-list) M)
      pen-down
      set pen-size 1
      set color 65 + group * 60
      set t-counter 1
      while [ t-counter < (length ( [opinion-list] of turtle 1 )) ] [
        setxy t-counter (confine-opinion-scale-to-max-pycor (item t-counter opinion-list) M)
        set t-counter t-counter + 1
      ]
    ]
  ]
end

;; REPORTERS

to-report new_opinion
  report max list (0 - M) (min list M (random-normal 0 1) - (initial_groupspread / 2) + (initial_groupspread * group))
end

to-report opinion_change [a s me]
  let core me - eta * a
  let discrepancy abs (me - a)
  let polarity_factor compute_polarity a
  let motcog ifelse-value (motivated_cognition != "no") [motivated_cognition_factor discrepancy lambda k] [1]
  let boomerang ifelse-value (motivated_cognition = "with acceptance latitude and boomerang") [(lambda ^ k - discrepancy ^ k) / (lambda ^ k + discrepancy ^ k)] [1]
  report max list (0 - M - a) (min list (M - a) (alpha * s * core * polarity_factor * motcog * boomerang))
end

to-report motivated_cognition_factor [d l kk]
  report (l) ^ kk / (l ^ kk + d ^ kk)
end

to-report compute_polarity [a]
  report ifelse-value (polarity) [max list (0) (M ^ b - (abs a) ^ b) / M ^ b] [1]
end

to-report confine-opinion-scale-to-max-pycor [x ma]
  ;; confines values to certain bounds, values exceeding the bound are set to the bound
  ;; x is the value to confine, ma is the absolute value of the maximal values to display
  ;; scales x such that it coincides with the bounds maxpycor and -maxpycor
  let y max list (min list x (ma - 0.0000000001)) (- ma + 0.0000000001)
  report y * max-pycor / ma ;; set to 0.999999999 instead of 1 to make these opinions visible in opinion histogram
end

to-report colorcode [x max_x]
  report hsb (270 - 270 * (min list (x / max_x) 1 )) 100 100
end

to-report sign [x]
  report abs x / x
end

to-report fragmentation
  r:put "x" [opinion] of turtles
  r:put "M" M
  report r:get "fragmentation(x, bw = 0.1, dx = 0.01, from=-M, to=M)"
end

to-report extremity
  report sqrt mean [opinion ^ 2] of turtles
end

to-report fraction_extremists
  report count turtles with [abs opinion > M - 0.1] / N
end

to set_baseline
  set N 500
  set eta 0
  set alpha 0.2
  set theta 0.01
  set M 3.5
  set motivated_cognition "no"
  set lambda 0.5
  set k 2
  set polarity false
  set b 2
  set intergroup_credibility 1
  set theta_as "probability for new random"
  set initial_groupspread 0
end

to scenario [name]
  set_baseline
  setup
  if run_and_export_figs [
    run_until_world_full
    export-interface (word "netlogofigs/" name "_interface.png")
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
514
223
1183
498
-1
-1
3.29
1
10
1
1
1
0
0
0
1
0
200
-40
40
1
1
1
ticks
30.0

BUTTON
418
35
479
68
Setup
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

BUTTON
481
35
542
68
Run!
set draw? true\ngo
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
14
90
164
123
N
N
5
1000
500.0
1
1
NIL
HORIZONTAL

TEXTBOX
17
278
222
296
1. Change strength
12
0.0
1

CHOOSER
418
70
565
115
visualization
visualization
"Heatmap timeline" "Agents' trajectories"
0

PLOT
1182
341
1552
497
Histogram Attitudes
Current Attitude
NIL
-2.0
2.0
0.0
1.0
true
false
"" "set-plot-y-range 0 count turtles / 12.5\nset-plot-x-range 0 - M - 0.05  (M + 0.05) "
PENS
"default" 0.0909 1 -13345367 true "" "histogram [opinion] of turtles"

SLIDER
15
188
218
221
theta
theta
0
0.3
0.098
0.002
1
NIL
HORIZONTAL

SLIDER
828
70
985
103
iterations_per_tick
iterations_per_tick
1
50
1.0
1
1
NIL
HORIZONTAL

SLIDER
987
70
1112
103
skip_ticks_draw
skip_ticks_draw
1
20
10.0
1
1
NIL
HORIZONTAL

SLIDER
34
576
220
609
lambda
lambda
0.01
3.5
1.0
0.005
1
NIL
HORIZONTAL

TEXTBOX
20
73
170
91
Setup parameters
12
0.0
1

SWITCH
1190
139
1280
172
rolling
rolling
0
1
-1000

TEXTBOX
421
10
1016
33
B. Time evoultion of opinion landscapes / Trajectories of opinions
18
0.0
1

TEXTBOX
986
617
1673
641
Monitors of attitude distribution and aggregate attitudes over time
18
0.0
1

TEXTBOX
10
10
372
66
Attitude dynamics
18
0.0
1

TEXTBOX
12
48
162
70
A. Parameters
18
0.0
1

TEXTBOX
847
106
960
124
Show longer trajectory
9
0.0
1

SWITCH
713
47
825
80
focus_one
focus_one
1
1
-1000

TEXTBOX
988
105
1112
129
Reduce grapic updates
9
0.0
1

TEXTBOX
1444
12
1594
34
D. Examples
18
0.0
1

TEXTBOX
1442
41
1711
59
Set parameters for certain examles
12
0.0
1

TEXTBOX
1566
17
1750
35
(Don't forget to click \"Run!\")
12
15.0
1

SLIDER
828
36
972
69
color_axis_max
color_axis_max
0.01
0.4
0.15
0.01
1
NIL
HORIZONTAL

BUTTON
715
82
825
116
Change focus
change_focus
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
454
452
514
497
min
- M
17
1
11

TEXTBOX
446
356
511
384
neutral = 0
11
0.0
1

SLIDER
15
297
144
330
alpha
alpha
0
1
0.2
0.01
1
NIL
HORIZONTAL

CHOOSER
15
528
221
573
motivated_cognition
motivated_cognition
"no" "latitude of acceptance" "latitude of acceptance and boomerang"
0

PLOT
226
526
399
646
change function
message
NIL
0.0
1.0
0.0
0.2
true
false
"" "clear-plot\nset-plot-x-range 0 2.5 * lambda\nset-plot-y-range 0 alpha * 2.5 * lambda"
PENS
"" 1.0 0 -16777216 true "" "foreach ( n-values 100 [ [x] -> x / 100 * 2.5 * lambda ] ) [ [x] -> plotxy x ( opinion_change 0 1 x) ]"

SLIDER
34
611
220
644
k
k
0
35
2.0
1
1
NIL
HORIZONTAL

SLIDER
152
299
348
332
intergroup_credibility
intergroup_credibility
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
14
125
163
158
initial_groupspread
initial_groupspread
0
4
0.0
0.05
1
NIL
HORIZONTAL

SLIDER
113
463
213
496
b
b
0
30
2.0
1
1
NIL
HORIZONTAL

SLIDER
13
426
176
459
M
M
0.1
10
3.5
0.1
1
NIL
HORIZONTAL

SWITCH
13
463
111
496
polarity
polarity
1
1
-1000

BUTTON
289
650
399
683
Update Plots
every 0.1 [\n  clear-all-plots\n  update-plots\n  ]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
226
399
398
519
polarity factor
attitude
NIL
0.0
0.0
0.0
1.2
true
false
"" "clear-plot"
PENS
"default" 1.0 0 -16777216 true "" "foreach ( n-values 200 [ [x] -> (x - 100) / 100 * 1.5 * M ] ) [ [x] -> plotxy x ( compute_polarity x) ]"

BUTTON
571
81
700
114
Redraw World
draw_trajectories
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
545
35
686
68
Run until world full
run_until_world_full
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
136
653
286
683
Click to play with parameters before run:
12
0.0
1

CHOOSER
15
224
220
269
theta_as
theta_as
"probability for new random" "probability for initial attitude" "weight on initial attitude"
0

MONITOR
454
223
514
268
max (M)
M
17
1
11

SLIDER
89
362
214
395
eta
eta
0
1
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
221
373
371
391
integration
12
0.0
1

TEXTBOX
20
373
86
391
contagion
12
0.0
1

MONITOR
976
663
1033
708
bw
bw
3
1
11

BUTTON
1450
59
1513
92
1-A
set_baseline\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1517
59
1580
92
1-B
set_baseline\nset eta 0\nset theta 0.17\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1583
59
1646
92
1-C
set_baseline\nset eta 0\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1112
179
1186
224
extremity
cum_extremity / M
3
1
11

MONITOR
1614
456
1769
501
NIL
imbalance-extremists
3
1
11

BUTTON
1517
95
1580
128
2-B
set_baseline\nset eta 0.5\nset motivated_cognition \"latitude of acceptance\"\nset lambda 0.25\nset k 2\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1449
131
1512
164
3-A
set_baseline\nset motivated_cognition \"latitude of acceptance\"\nset lambda 0.5\nset k 10\nsetup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1449
95
1512
128
2-A
set_baseline\nset initial_groupspread 3\nset intergroup_credibility 0.1\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1583
94
1646
127
2-C
set_baseline\nset eta 0\nset motivated_cognition \"latitude of acceptance\"\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1517
131
1580
164
3-B
set_baseline\nset eta 0.9\nset motivated_cognition \"latitude of acceptance\"\nset lambda 0.5\nset k 10\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1583
130
1646
163
3-C
set_baseline\nset eta 0.75\nset motivated_cognition \"latitude of acceptance\"\nset k 10\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
16
169
166
187
Independent attitudes
12
0.0
1

TEXTBOX
159
276
309
294
2. Source credibility
12
0.0
1

TEXTBOX
16
341
166
359
3. Attitude change
12
0.0
1

TEXTBOX
16
404
166
422
4. Polarity
12
0.0
1

TEXTBOX
21
511
171
529
5. Motivated cognition
12
0.0
1

BUTTON
1564
568
1789
601
Extremism through polarity
set_baseline\nset alpha 0.5\nset motivated_cognition \"latitude of acceptance\"\nset latitude_acceptance 2\nset sharpness_acceptance 20\nset polarity true\nset M 2\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1182
178
1552
341
Output measures (normalized)
NIL
NIL
0.0
1.0
0.0
1.0
false
true
"" "ifelse rolling [\n  ifelse ticks > (max-pxcor)\n    [set-plot-x-range (ticks - max-pxcor) ticks]\n    [set-plot-x-range 0 max-pxcor]\n  ] [\n  ifelse ticks > (max-pxcor)\n    [set-plot-x-range 0 ticks]\n    [set-plot-x-range 0 max-pxcor]\n  ]"
PENS
"bias" 1.0 0 -2674135 true "" "plot abs mean [opinion] of turtles / M"
"std. dev." 1.0 0 -16777216 true "" "plot standard-deviation [opinion] of turtles / M"
"fragmentation" 1.0 0 -13791810 true "" "plot fragmentation / M / 4"
"extremity" 1.0 0 -5825686 true "" "plot extremity / M"
"initial std. dev" 1.0 0 -7500403 true "" "plot 1 / M"

SLIDER
1009
140
1181
173
spinup_tick
spinup_tick
1
200
50.0
1
1
NIL
HORIZONTAL

MONITOR
1000
179
1057
224
std.dev.
cum_sd / M
3
1
11

MONITOR
1055
179
1112
224
frag.
cum_fragmentation / M / 4
3
1
11

MONITOR
944
179
1001
224
bias
cum_biasmean / M
3
1
11

TEXTBOX
840
501
897
519
time -->
12
0.0
1

TEXTBOX
458
297
509
339
positive attitudes
11
0.0
1

TEXTBOX
457
397
508
425
negative attitudes
11
0.0
1

MONITOR
1603
634
1708
679
NIL
fragmentation
17
1
11

MONITOR
533
178
583
223
NIL
alpha
17
1
11

MONITOR
582
178
632
223
NIL
eta
17
1
11

MONITOR
631
178
681
223
NIL
theta
17
1
11

TEXTBOX
457
186
547
222
Input parameters
12
0.0
1

TEXTBOX
875
190
941
218
Output measures
12
0.0
1

MONITOR
680
129
735
174
NIL
lambda
2
1
11

MONITOR
733
129
783
174
NIL
k
1
1
11

MONITOR
780
129
830
174
NIL
b
1
1
11

MONITOR
728
613
868
658
NIL
fraction_extremists
3
1
11

@#$#@#$#@
# Continuous Opinion Dynamics under Bounded Confidence with Dyadic Interaction, Heterogeneous Bounds of Confidence and Independent Opinion Formation

## WHAT IS IT?

This is model of **continuous opinion dynamics under bounded confidence**, which focusses on

  * heterogeneous bounds of confidence coming from a two parameter [beta distribution](http://en.wikipedia.org/wiki/Beta_distribution) in a mean/dispresion paramterization
  * a dyadic communication regime very similar to [Deffuant et al 2000](http://dx.doi.org/10.1142/S0219525900000078)
  * independent opinion formation as a new draw from the intial distribution with a small probability as introduced in [Pineda et al 2009](http://dx.doi.org/10.1088/1742-5468/2009/08/P08001)), additionally independent opinion formation can also be set to be a switch back to the original initial opinion

It has visualizations for:

  * a bar plot histogram of the different static bounds of confidence (eps)
  * a rolling colored histogram (heatmap) of opinion density over time
  * rolling trajectories of opinions over time colored by the bound of confidence
  * it can additionally focus on the trajectory of a randomly chosen agent
  * a bar plot histogram of current opinions either with many or with 11 bins, the latter is analog to left-right ideological positions in survey questionaires as the European Social Survey
  * trajectories of the mean and the median opinion over time

## HOW IT WORKS

### In a nutshell

Agents are selected randomly to adjust their opinion which are numbers between zero and one. With a certain (typically small) probability a selected agent chooses independent opinion formation which is a random draw. Otherwise agents choose interdependent opinion formation where the agent adjusts its opinion gradually towards the opinions of another randomly selected agent but only when the distance in opinion is within its bound of confidence.

### Variables

Each of N agent has its **opinion** between 0.0 and 1.0 as a dynamic variable and its **bound of confidence** (eps) as a static variable. The global static variables are
  * mu - the mean of the beta distribution where confidence bounds are drawn from
  * sigma - the standard deviaition  of the beta distribution where confidence bounds are drawn from
  * p - the probability if an agent is doing independent insteadt of interdependent opinion formation

### Initialization of agent variables

Each agent is assigned its initial opinion as a random number between 0.0 and 1.0 from the uniform distribution. Each agent is assigned its bound of confidence as a random number form a [beta-distribution](http://en.wikipedia.org/wiki/Beta_distribution) with mean mu and standard deviation sigma.

## HOW TO USE IT

Click "Setup" to inititialize agents with opinions random and uniformly distributed between 0.0 and 1.0. Agents are located at the left border of the world with their opinions spreading over the vertical axis. Further, on confidence bounds (eps) are initialized for each agent as random draws from a beta distribution under the current choice of mu and sigma.

Click "Go" to start the simulation. With each tick, agents move from left to right displaying their opinion with the position on the vertical axis. This goes over into a "rolling" graphic in the world, where the last ticks are shown.

Visualization can be chosen as trajectories of agents or as heatmaps of the density (color-coded histograms). In heatmaps each patch's color is associated to the number of agents at this patch. Further switches, choosers, and sliders can be used to change the visualization.

A change of N, mu and sigma is only effective at setup. A change of p is effective at runtime.


## THINGS TO NOTICE

Agents move towards the right-hand side of the world with one step each tick. This goes over into a "rolling" plot.

When you click "Setup" see how the distribution of the bounds of confidence (eps) looks like under different mu and sigma in the **bar plot histogram of eps**. See what happens when sigma is small and large.

Notice how agents form **clusters** in the opinion space. See how under what conditions how many clusters **evolve**, when they **drift**, and **unite** in the "**Heatmap**"-visualization.

Look at the role of agents with different bounds of confidence in the "**Agents' trajectories**"-visualization.

In either visualization, focus additionally on the **trajectory of an individual agent** and see how it jumps between clusters when the p is not zero.

Increase iterations_per_tick to see what happens in the really long run.

Look at the current distribution of opinions in the **bar plot histogram** on the right hand side and compare it to the colored histogram (the most recent colored vertical line in the world at the right hand side).

Look how the **mean and the median opinion** evolve over time. The mean represents the center of mass of the distribution. The median represents an unbeatable opinion under pairwise majority decisions. (This holds when agents have single-peaked preferences with peaks at their opinion, cf. [median voter theorem](http://en.wikipedia.org/wiki/Median_voter_theorem)).


## THINGS TO TRY

Try the example buttons to explore characteristic behavior and try to systematically understand the role of average open-mindedness (mu), dispersion of open- and closed-mindedness (sigma), and the degree of independent opinion formation (m).


## RELATED MODELS AND PAPERS

**Original HK and DW models**
Hegselmann, R. & Krause, U. [Opinion Dynamics and Bounded Confidence, Models, Analysis and Simulation](http://jasss.soc.surrey.ac.uk/5/3/2.html) Journal of Artificial Societies and Social Simulation, 2002, 5, 2
Deffuant, G.; Neau, D.; Amblard, F. & Weisbuch, G. [Mixing Beliefs among Interacting Agents](http://dx.doi.org/10.1142/S0219525900000078) Advances in Complex Systems, 2000, 3, 87-98
Weisbuch, G.; Deffuant, G.; Amblard, F. & Nadal, J.-P. [Meet, discuss, and segregate!](http://dx.doi.org/10.1002/cplx.10031) Complexity, 2002, 7, 55-63

**General model including HK and DW**
Urbig, D.; Lorenz, J. & Herzberg, H. [Opinion dynamics: The effect of the number of peers met at once](http://jasss.soc.surrey.ac.uk/11/2/4.html) Journal of Artificial Societies and Social Simulation, 2008, 11, 4

**On noise:**
Pineda, M.; Toral, R. & Hernandez-Garcia, E. [Noisy continuous-opinion dynamics](http://stacks.iop.org/1742-5468/2009/P08001) Journal of Statistical Mechanics: Theory and Experiment, 2009, 2009, P08001 (18pp)
Mäs, M.; Flache, A. & Helbing, D. [Individualization as Driving Force of Clustering Phenomena in Humans](http://dx.doi.org/10.1371/journal.pcbi.1000959) PLoS Comput Biol, Public Library of Science, 2010, 6, e1000959

**On heterogeneous bounds of confidence**
Lorenz, J. [Heterogeneous bounds of confidence: Meet, Discuss and Find Consensus!](http://dx.doi.org/10.1002/cplx.20295) Complexity, 2010, 15, 43-52

**On extremism**
Deffuant, G.; Neau, D.; Amblard, F. & Weisbuch, G. [How Can Extremism Prevail? A Study Based on the Relative Agreement Interaction Model](http://jasss.soc.surrey.ac.uk/5/5/1.html) Journal of Artificial Societies and Social Simulation, 2002, 5, 1
Deffuant, G. [Comparing Extremism Propagation Patterns in Continuous Opinion Models](http://jasss.soc.surrey.ac.uk/9/3/8.html) Journal of Artificial Societies and Social Simulation, 2006, 9, 8

**Survey, Motivation and Variation**
Lorenz, J. [Continuous Opinion Dynamics under bounded confidence: A Survey](http://dx.doi.org/10.1142/S0129183107011789) Int. Journal of Modern Physics C, 2007, 18, 1819-1838
Urbig, D. [Attitude Dynamics with Limited Verbalisation Capabilities](http://www.jasss.surrey.ac.uk/6/1/2.html) Journal of Artificial Societies and Social Simulation, 2003, 6, 2
Lorenz, J. & Urbig, D. [About the Power to Enforce and Prevent Consensus by Manipulating Communication Rules](http://dx.doi.org/10.1142/S0219525907000982) Advances in Complex Systems, 2007, 10, 251
Amblard, F. & Deffuant, G. [The role of network topology on extremism propagation with the relative agreement opinion dynamics](http://dx.doi.org/10.1016/j.physa.2004.06.102) Physica A: Statistical Mechanics and its Applications, 2004, 343, 725-738
Groeber, P.; Schweitzer, F. & Press, K. [How Groups Can Foster Consensus: The Case of Local Cultures](http://jasss.soc.surrey.ac.uk/12/2/4.html) Journal of Artificial Societies and Social Simulation, 2009, 12, 4




## CREDITS AND REFERENCES
This is the extended version of "Continuous Opinion Dynamics under Bounded Confidence" from 2012

Copyright 2015 Jan Lorenz. http://janlo.de, post@janlo.de

![Creative Commons Attribution-ShareAlike 3.0 Unported License](https://i.creativecommons.org/l/by-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ .
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

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

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

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

cylinder
false
0
Circle -7500403 true true 0 0 300

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

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

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

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="delta_p2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>run_until_world_full</go>
    <timeLimit steps="1"/>
    <metric>cum_fragmentation</metric>
    <metric>cum_sd</metric>
    <metric>cum_biasmean</metric>
    <enumeratedValueSet variable="initial_groupspread">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="independent_distribution">
      <value value="&quot;normal (mean 0  sd 1)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="latitude_acceptance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="color_axis_max">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sharpness_acceptance">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0"/>
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.06"/>
      <value value="0.07"/>
      <value value="0.08"/>
      <value value="0.09"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eta">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
      <value value="0.75"/>
      <value value="0.8"/>
      <value value="0.85"/>
      <value value="0.9"/>
      <value value="0.91"/>
      <value value="0.92"/>
      <value value="0.93"/>
      <value value="0.94"/>
      <value value="0.95"/>
      <value value="0.96"/>
      <value value="0.97"/>
      <value value="0.98"/>
      <value value="0.99"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="polarity">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focus_one">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="M">
      <value value="3.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_as">
      <value value="&quot;probability for new random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated_cognition">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rolling">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skip_ticks_draw">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="iterations_per_tick">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;Heatmap timeline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intergroup_credibility">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="latitude_sharpness" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>run_until_world_full</go>
    <timeLimit steps="1"/>
    <metric>cum_biasmean</metric>
    <metric>cum_sd</metric>
    <metric>cum_fragmentation</metric>
    <enumeratedValueSet variable="initial_groupspread">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="independent_distribution">
      <value value="&quot;normal (mean 0  sd 1)&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="latitude_acceptance" first="0.1" step="0.05" last="1.4"/>
    <enumeratedValueSet variable="color_axis_max">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sharpness_acceptance">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0"/>
      <value value="0.01"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="polarity">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focus_one">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="M">
      <value value="3.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_as">
      <value value="&quot;probability for new random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="motivated_cognition">
      <value value="&quot;latitude of acceptance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rolling">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skip_ticks_draw">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="iterations_per_tick">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;Heatmap timeline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="intergroup_credibility">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spinup_tick">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
