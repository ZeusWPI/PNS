class_name Server
extends Node2D

var _thread: Thread
var _thread_exit_requested: bool

var _peers: Array[PacketPeerUDP]
var _main

func _init(main):
	_thread = Thread.new()
	_thread_exit_requested = false
	_peers = []
	_main = main
	
	_thread.start(_serve)
	
func _log(message: String):
	print("[%s] [SERVER] %s" % [_thread.get_id(), message])

func _serve():
	_log("Serving...")
	var socket: UDPServer = UDPServer.new()
	
	socket.max_pending_connections = 4 #FIXME: INCREASING THIS SEVERLY BREAKS THINGS UNDER LOAD
	socket.listen(12512)
	
	
	while not _thread_exit_requested:
		socket.poll()
		
		if socket.is_connection_available():
			while socket.is_connection_available():
				_peers.append(socket.take_connection())
		
		for peer in _peers:
			if peer.get_available_packet_count() > 0:
				var packet = peer.get_packet()
				_main.draw_packet(Color(randf(), randf(), randf()))
			pass
		
		OS.delay_msec(1)

func exit():
	_thread_exit_requested = true
	_thread.wait_to_finish()
