extends Node2D

const margin: int = 200
var color: Color

func set_color(color: Color):
	self.color = color

func _ready():
	$CPUParticles2D.emitting = true
	$CPUParticles2D.one_shot = true
	$CPUParticles2D.color = self.color
	
	position.x = margin + randi() % roundi(get_viewport().get_visible_rect().size.x - 2 * margin)
	position.y = margin + randi() % roundi(get_viewport().get_visible_rect().size.y - 2 * margin)

func _process(delta):
	if not $CPUParticles2D.emitting:
		queue_free()
