defmodule WC do
  use Witchcraft

  import Algae
  import TypeClass
  import Destructure

  defdata Flux do
    left :: :alphanumeric | :whitespace
    counter :: integer()
    right :: :alphanumeric | :whitespace
  end

  alias WC.Flux

  defimpl TypeClass.Property.Generator, for: Flux do
    # defp character_type, do: Enum.random([:alphanumeric, :whitespace])

    def generate(_),
      do: %Flux{left: :whitespace, right: :whitespace, counter: Enum.random(-10..10)}
  end

  definst Witchcraft.Semigroup, for: Flux do
    def append(
          d(%Flux{left, counter: c1, right: :alphanumeric}),
          d(%Flux{right, counter: c2, left: :alphanumeric})
        ) do
      d(%Flux{left, right, counter: c1 <> c2 <> -1})
    end

    def append(
          d(%Flux{left, counter: c1, right: _}),
          d(%Flux{right, counter: c2, left: _})
        ) do
      d(%Flux{left, right, counter: c1 <> c2})
    end
  end

  definst Witchcraft.Monoid, for: Flux do
    def empty(%Flux{left: _, counter: c, right: _}),
      do: %Flux{left: :whitespace, counter: Witchcraft.Monoid.empty(c), right: :whitespace}
  end

  def is_space(c), do: c in [" ", "\t", "\n"]

  def is_newline(c), do: (c == "\n" && 1) || 0

  def flux(c) do
    if is_space(c) do
      Flux.new()
    else
      Flux.new(:alphanumeric, 1, :alphanumeric)
    end
  end

  def process(file_path) do
    file_path
    |> File.stream!([], 1)
    # |> Stream.scan({0,0,WC.Flux.new()}, fn char, acc ->
    #   Witchcraft.Semigroup.append({1, is_newline(char), flux(char)}, acc)
    # end)
    |> Enum.reduce({0, 0, Flux.new()}, fn char, acc ->
      {1, is_newline(char), flux(char)} <> acc
    end)
    |> (fn {chars, lines, %Flux{counter: words}} -> d(%{chars, lines, words}) end).()
    |> IO.inspect()
  end
  # TODO: Fix wrong output
  # def process(file_path) do
  #   file_path
  #   |> File.stream!()
  #   |> Stream.flat_map(&String.split(&1, ""))
  #   |> Enum.reduce({0, 0, Flux.new()}, fn char, acc ->
  #     {is_empty(char), is_newline(char), flux(char)} <> acc
  #   end)
  #   |> (fn {chars, lines, %Flux{counter: words}} -> d(%{chars, lines, words}) end).()
  #   |> IO.inspect()
  # end
end
end
