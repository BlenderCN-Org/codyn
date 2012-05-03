TIME=0:0.01:1000

KINEMATICS=system . has-template(physics.body) . /q|q_ref|q_tot/
VISUAL=system . has-template(physics.body) . points
TORQUES=system . has-template(physics.body) . tau
LEARNING=system . has-template(physics.body) . learning . /signal|output|rdot/
WEIGHTS=system . has-template(physics.body) . learning . r

DEBUG=

pendulum:
	@$(DEBUG) cdn-monitor			\
		-t $(TIME) 			\
		-i				\
		-m "$(VISUAL)" 			\
		-o visual.txt 			\
		-m "$(KINEMATICS)"		\
		-o kinematics.txt		\
		-m "$(TORQUES)"			\
		-o torques.txt			\
		-m "$(LEARNING)"		\
		-o learning.txt			\
		-m "$(WEIGHTS)"			\
		-o weights.txt			\
		pendulum.cdn

learn:
	@cdn-monitor				\
		-t $(TIME)			\
		-i				\
		-m s.signal			\
		-m 's."output"'			\
		-o out.txt
