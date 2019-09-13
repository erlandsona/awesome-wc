use Witchcraft

import Algae
import TypeClass
import Destructure

defdata Flux do
  left :: :alphanumeric | :whitespace
  counter :: integer()
  right :: :alphanumeric | :whitespace
end

defimpl TypeClass.Property.Generator, for: Flux do
  defp character_type, do: Enum.random([:alphanumeric, :whitespace])

  def generate(_), do: %Flux{left: character_type(), right: character_type(), counter: Enum.random(-10..10)}
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
  def empty(d(%Flux{right, counter, left})) do
    d(%Flux{left, right, counter: Witchcraft.Monoid.empty(counter)})
  end
end

const = fn a, _b -> a end

is_space = &((&1 == '\n' && 1) || 0)

flux = fn c ->
  if is_space.(c) do
    Flux.empty()
  else
    Flux.new(:alpha, 1, :alpha)
  end
end

System.argv()
|> Enum.map(fn file_path ->
  file_path
  |> File.stream!()
  |> Stream.transform({0, 0, 0}, fn char ->
    {const.(1, char), is_space.(char), flux.(char)}
  end)
  |> Enum.reduce(&Witchcraft.Semigroup.append/2)
end)
