TIME=0:0.001:10

KINEMATICS=system . has-template(physics.body) . /q/
TORQUES=system . has-template(physics.body) . tau

DEBUG=

pendulum:
	@$(DEBUG) cdn-monitor			\
		-t $(TIME) 			\
		-i				\
		-m "$(KINEMATICS)"		\
		-o kinematics.txt		\
		-m "$(TORQUES)"			\
		-o torques.txt			\
		pendulum.cdn
