import Config

config :logger, level: :info

if Mix.env() == :dev do
  config :mix_test_watch,
    exclude: [~r/.*temp_delete_contents.*/]
end

import_config "#{config_env()}.exs"
