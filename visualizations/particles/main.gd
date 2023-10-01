extends Node

var server: Server

var packet = preload("res://Packet.tscn")

func _ready():
	server = Server.new(self)

func draw_packet(color: Color):
	_draw_packet.call_deferred(color)

func _draw_packet(color: Color):
	var p = packet.instantiate()
	p.set_color(color)
	add_child(p)

func _exit_tree():
	server.exit()
