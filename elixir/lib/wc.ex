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
      d(%Flux{left, right, counter: (c1 <> c2) <> -1})
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
    |> File.stream!(read_ahead: 100_000)
    |> Flow.from_enumerable()
    |> Flow.flat_map(&String.graphemes/1)
    #  FIXME: partitioning messes with the word count, :shrug:
    # |> Flow.partition()
    |> Flow.reduce(fn -> [{0, 0, Flux.new()}] end, fn char, [acc] ->
        [{1, is_newline(char), flux(char)} <> acc]
    end)
    |> Enum.reduce(&Witchcraft.Semigroup.append/2)
    |> (fn {chars, lines, %Flux{counter: words}} -> d(%{chars, lines, words}) end).()
  end
end
