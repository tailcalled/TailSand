TailSand is a [falling sand game](https://en.wikipedia.org/wiki/Falling-sand_game) (particle-based sandbox game) primarily created as a host for its relation-based modding language. As a falling sand game, it simulates particles (such as sand, water, fire, and more) which interact and move, and which can be painted or erased to the sandbox. Due to its modding language, the behavior of the particles can be specified using a domain-specific language, whose primary unique feature is its ability to define custom relations between particles as an abstraction mechanism.

The scripting language consists of a series of declarations. The simplest such declaration is an element declaration, which looks like the following:

    element Wall #808080;

This declares a gray particle named `Wall` which, until more has been defined, does nothing. To make it act, a number of properties can be defined, including `gravity` (makes things fall), `spread` (makes things spread out as they fall), `density` (decides the ordering that materials end up in when mixed), `slide` (makes things spread out evenly, like a liquid), `tag` (defines the how the user accesses an element). Furthermore, interactions that change the elements can also be defined; there is `self` (makes an element turn into another kind of element), and `reaction` (makes two elements react with each other to produce two new elements).

Much more interesting, though, than the built-in properties, is how users can declare their own properties. For instance, we might want the `Wall` element to behave as a solid, but it would be annoying to type out all the required properties for this for each element that we declare solid. To avoid this, we can declare a relation that makes elements solid:

    relation Solid/1;
    
    where Solid 'x:
        tag 'x "Solids";
        density 'x 1.0;
        description 'x "Solid. ";
    end;
    
    Solid Wall;

The declaration `relation Solid/1;` defines a new relation, `Solid`, with arity `1`. After this has been declared, any part of the code can use a `where` block to define properties for any element declared solid. This means that rather than having to write a list of declarations along the lines of `density Wall 1.0;`, one can simply write `Solid Wall;`. The `Wall` element will then be filled in for the generic `'x` parameter (or any other generic parameters that might be used).

These blocks can be nested, and they can require elements to satisfy multiple relations, or have multiple generic parameters.

The full EBNF grammar for the language can be seen below:

    block ::= { declaration ';' }
    declaration ::= 'title' <<string>>
                 |  'element' <<ident>> <<color>>
                 |  'relation' <<ident>> '/' <<integer>>
                 |  'tag' <<ident>> <<string>>
                 |  'description' <<ident>> <<string>>
                 |  'name' <<ident>> <<string>>
                 |  'spread' <<ident>> <<number>>
                 |  'gravity' <<ident>> <<number>>
                 |  'self' <<number>> <<ident>> '=>' <<ident>>
                 |  'reaction' <<number>> <<ident>> <<ident>> '=>' <<ident>> <<ident>>
                 |  'where' constraint { ',' constraint } ':' block 'end'
                 |  <<ident>> { <<ident>> }
    constraint ::= <<ident>> { <<ident>> }