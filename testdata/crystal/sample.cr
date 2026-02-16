require "json"
require "http/server"
require "./models"

module MyApp
  VERSION = "1.0.0"
  MAX_CONNECTIONS = 100

  alias JsonHash = Hash(String, JSON::Any)

  enum Status
    Active
    Inactive
    Archived
  end

  class Config
    property name : String
    property port : Int32
    property debug : Bool

    def initialize(@name, @port, @debug = false)
    end

    def self.from_json(data : String) : Config
      parsed = JSON.parse(data)
      new(
        name: parsed["name"].as_s,
        port: parsed["port"].as_i,
        debug: parsed["debug"].as_bool
      )
    end

    def to_s
      "Config(#{@name}:#{@port})"
    end

    private def validate
      raise "Invalid port" if @port < 0
    end
  end

  struct Point
    property x : Float64
    property y : Float64

    def initialize(@x, @y)
    end

    def distance_to(other : Point) : Float64
      Math.sqrt((@x - other.x) ** 2 + (@y - other.y) ** 2)
    end
  end

  def self.start_server(config : Config)
    server = HTTP::Server.new do |context|
      context.response.content_type = "text/plain"
      context.response.print "Hello!"
    end
    server.bind_tcp(config.port)
    server.listen
  end

  protected def helper_method
    # internal use
  end
end
