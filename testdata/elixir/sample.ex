defmodule MyApp.Accounts.UserServer do
  @moduledoc """
  GenServer for managing user sessions and state.
  """

  use GenServer
  use MyApp.Telemetry

  alias MyApp.Accounts.User
  alias MyApp.Repo
  alias MyApp.Cache, as: AppCache
  import Ecto.Query
  import Ecto.Changeset, only: [cast: 3, validate_required: 2]
  require Logger

  @callback handle_event(event :: term(), state :: term()) :: {:ok, term()} | {:error, term()}
  @callback on_connect(user_id :: integer()) :: :ok | {:error, term()}

  @default_timeout 5_000
  @max_retries 3

  defstruct [:user_id, :session_token, :connected_at, status: :idle]

  # Public API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def get_user(user_id) do
    GenServer.call(__MODULE__, {:get_user, user_id})
  end

  def update_user(user_id, attrs) do
    GenServer.call(__MODULE__, {:update_user, user_id, attrs}, @default_timeout)
  end

  def list_active_sessions do
    GenServer.call(__MODULE__, :list_active)
  end

  def broadcast_message(message) do
    GenServer.cast(__MODULE__, {:broadcast, message})
  end

  # Private helpers

  defp fetch_from_cache(user_id) do
    case AppCache.get("user:#{user_id}") do
      nil -> {:error, :not_found}
      user -> {:ok, user}
    end
  end

  defp validate_attrs(attrs) do
    required = [:name, :email]
    Enum.all?(required, &Map.has_key?(attrs, &1))
  end

  defp build_query(filters) do
    from(u in User,
      where: ^filters,
      order_by: [desc: u.inserted_at],
      preload: [:profile]
    )
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    Logger.info("Starting UserServer with opts: #{inspect(opts)}")
    {:ok, %{sessions: %{}, opts: opts}}
  end

  @impl true
  def handle_call({:get_user, user_id}, _from, state) do
    case fetch_from_cache(user_id) do
      {:ok, user} -> {:reply, {:ok, user}, state}
      {:error, _} ->
        user = Repo.get(User, user_id)
        {:reply, {:ok, user}, state}
    end
  end

  @impl true
  def handle_call({:update_user, user_id, attrs}, _from, state) do
    with {:ok, user} <- fetch_from_cache(user_id),
         changeset <- User.changeset(user, attrs),
         {:ok, updated} <- Repo.update(changeset) do
      {:reply, {:ok, updated}, state}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_cast({:broadcast, message}, state) do
    Enum.each(state.sessions, fn {_id, pid} ->
      send(pid, {:message, message})
    end)
    {:noreply, state}
  end

  defmacro define_handler(name, do: block) do
    quote do
      def handle_event(unquote(name), state) do
        unquote(block)
      end
    end
  end
end

defmodule MyApp.Accounts.UserServer.Helpers do
  @moduledoc false

  def format_session(session) do
    %{
      id: session.user_id,
      status: session.status,
      connected: session.connected_at
    }
  end
end
