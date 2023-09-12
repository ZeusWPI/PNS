extends Node2D

var ballsc = preload("res://Ball.tscn")
var rng = RandomNumberGenerator.new()
var balls = []
# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
#func _process(delta):
#	pass

func _input(event):
	# Mouse in viewport coordinates.
	if event is InputEventMouseButton:
		spawn_ball()


func spawn_ball():
	var ball: Node2D = ballsc.instantiate()
	add_child(ball)
	var x = rng.randi_range(100, 900)
	ball.position = Vector2(x, 0)
	
	balls.push_back(ball)
	if balls.size() > 500:
		var old_ball = balls.pop_front()
		old_ball.queue_free()
