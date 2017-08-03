# purescript-pux-form

A library to generate forms for [Pux](http://purescript-pux.org) with lens.

## Installation

```shell
bower install purescript-pux-form
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-pux-form).

## Example

Example code is avaliable at https://github.com/b123400/purescript-pux-form-example

Live version of the example: https://b123400.github.io/purescript-pux-form-example/

## Tutorial

Normally when you are making a form, you need to create Events like `UpdateUsername` `UpdatePassword` which are binded to form elements, and have to implement related handlers in `foldp`. This library handles the form generation, all you need is a simple `Update` event.

### Prepare the state

```purescript
newtype State = State
  { name :: String
  , password :: String
  , biography :: String
  , age :: Int
  , alive :: Boolean
  }

-- Create lens for record
name :: Lens' State String
name = lens (\(State s)-> s.name) (\(State s) val-> State s { name = val })

password :: Lens' State String
password = lens (\(State s)-> s.password) (\(State s) val-> State s { password = val })

biography :: Lens' State String
biography = lens (\(State s)-> s.biography) (\(State s) val-> State s { biography = val })

age :: Lens' State Int
age = lens (\(State s)-> s.age) (\(State s) val-> State s { age = val })

alive :: Lens' State Boolean
alive = lens (\(State s)-> s.alive) (\(State s) val-> State s { alive = val })
```

### Setup events

```purescript
data Event = Replace State
foldp (Replace s) _ = noEffects s
```

### Update the view to render the form

```purescript
view :: State -> HTML Event
view (s@State s') =
  div do
    p $ text ("Name: "         <>      s'.name)
    p $ text ("Age: "          <> show s'.age)
    p $ text ("Biography: "    <>      s'.biography)
    p $ text ("Password: "     <>      s'.password)
    p $ text ("Alive? "        <> show s'.alive)
    form s fields Replace
  where fields = name                   .\ "Enter name"
              <> age                    .\ "Enter age"    -- Number input field
              <> (age <<< asRange 10 100 2) .\ "Input age as Range"      -- Custom input field
              <> (password <<< asPassword)  .\ "Enter your password"     -- String in password field
              <> (biography <<< asTextArea) ./ (b $ text "Enter biography in text area") -- Label with custom HTML
              <> alive                  .\ "Are you alive?"

```

------

The `<input>` element's type is determined by the data type,

- String is rendered to `<input type="text">`
- Int, Num are rendered to `<input type="number">`
- Boolean is rendered to `<input type="checkbox">`

You can customize the input type by wrapping the lens.

- `asPassword` wraps `Lens s String` and is rendered as `<input type="password">`
- `asTextArea` wraps `Lens s String` and is rendered as `<textarea></textarea>`
- `asFile` wraps `Lens s String` and is rendered as `<input type="file">`
- `asRange` wraps `Lens s Int` and is rendered as `<input type="range">`
- `asRangeNum` wraps `Lens s Num` and is rendered as `<input type="range">`

### Constant Value

- `asConstButton` creates a button which sets the value to a constant value when clicked.

```purescript
field $ age <<< asConstButton 20 "Set age to 20"
```

### Multiple choice

Mutliple choice input can be created with `asDropdown` and `asDropdown'`.

```purescript

data Gender = Male | Female | Secret
newtype State = State { gender :: Gender }

instance multipleChoiceGender :: MultipleChoice Gender where
  choices = [Male, Female, Secret]

instance showGender :: Show Gender where
  show Male = "Male"
  show Female = "Female"
  show Secret = "Secret"

view s = form s (field $ gender <<< asDropdown) Replace
```

Or if it is `Show a` you can supply an Array of `a` to `asDropdown'`.

```purescript
gender <<< asDropdown' [Male, Female, Secret]
```
