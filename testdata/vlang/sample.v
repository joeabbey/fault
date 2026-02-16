module main

import os
import json
import net.http { Request, Response }

pub const version = '1.0.0'
const internal_limit = 100

pub struct Config {
pub:
	name  string
	port  int
pub mut:
	debug bool
}

struct InternalState {
	count int
}

pub enum Status {
	active
	inactive
	archived
}

pub interface Handler {
	handle(req Request) Response
}

pub type Callback = fn (string) bool

pub fn new_config(name string, port int) Config {
	return Config{
		name:  name
		port:  port
		debug: false
	}
}

pub fn (c Config) to_string() string {
	return 'Config(${c.name}:${c.port})'
}

fn internal_helper(x int, y int) int {
	return x + y
}

pub fn start_server(config Config) ! {
	mut server := http.Server{
		port: config.port
	}
	server.on_message(fn (msg string) {
		println(msg)
	})
	server.listen() or { panic(err) }
}
