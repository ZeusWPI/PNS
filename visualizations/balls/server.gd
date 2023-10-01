# server_node.gd
extends Node2D

var server := UDPServer.new()
var peers = []

func _ready():
	server.listen(12512, "0.0.0.0")

func _process(delta):
	server.poll() # Important!
	while server.is_connection_available():
		var peer: PacketPeerUDP = server.take_connection()
		var packet = peer.get_packet()
		peers.append(peer)

	for i in range(0, peers.size()):
		var peer = peers[i]
		if peer.get_available_packet_count() > 0:
			var pack = peer.get_packet()
			get_parent().spawn_ball()
