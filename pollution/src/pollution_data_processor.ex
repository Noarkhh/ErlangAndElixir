defmodule PollutionDataProcessor do
  defp parse_datetime(date_str, time_str) do
    date =
      date_str
      |> String.split("-")
      |> Enum.map(&(Integer.parse(&1) |> elem(0)))
      |> :erlang.list_to_tuple()

    time =
      (time_str <> ":00")
      |> String.split(":")
      |> Enum.map(&(Integer.parse(&1) |> elem(0)))
      |> :erlang.list_to_tuple()

    {date, time}
  end

  defp parse_location(latitude_str, longitude_str) do
    {Float.parse(latitude_str) |> elem(0), Float.parse(longitude_str) |> elem(0)}
  end

  defp parse_measurement(str) do
    [date, time, latitude, longitude, value] = String.split(str, ",")

    %{
      :datetime => parse_datetime(date, time),
      :location => parse_location(latitude, longitude),
      :pollution_level => Integer.parse(value) |> elem(0)
    }
  end

  defp identify_stations(measurements) do
    Enum.uniq_by(measurements, &Map.get(&1, :location))
  end

  def parse_pollution_measurements_file(csv_path) do
    File.read!(csv_path)
    |> String.split("\n")
    |> Enum.map(&parse_measurement/1)
  end

  def parse_pollution_file(csv_path) do
    measurements = parse_pollution_measurements_file(csv_path)
    stations =
      identify_stations(measurements)
      |> Enum.map(&Map.get(&1, :location))
      |> Enum.map(fn ({latitude, longitude}) -> %{:name => "station_#{latitude}_#{longitude}", :location => {latitude, longitude}} end)
    {stations, measurements}
  end
end