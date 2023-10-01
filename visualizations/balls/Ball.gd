extends RigidBody2D


# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	scale.x -= 0.1
	scale.y -= 0.1
	
	
	if scale.x <= 0.1:
		visible = false
