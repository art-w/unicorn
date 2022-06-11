Can GUI programming be purely functional? This library demonstrates a small algebra of 7 combinators with intuitive laws to make GUI programming super concise, declarative, easy to reason and efficient to use. It's a bold claim! As the humble name of this library suggests, I'm too biased to be trusted. You will have to judge by yourself if it delivers:

[**Try the interactive online tutorial**](https://art-w.github.io/unicorn/playground.html) | [**Documentation**](https://art-w.github.io/unicorn/unicorn_jsoo/Unicorn_jsoo/index.html)

This is a very early release focused on showing off the combinators API. A lot of features and optimizations are still missing for real world usage! Bugs are expected. You will get the most of it if you enjoy playing with weird new toys... or if you have an interest in the theoretical questions: How can widgets have internal state but also be referentially transparent?  What does it mean to "move" a pure widget? Can we do reactive GUIs without FRP or monads?

Unicorn took years of inspiration from previous works, ranging from the immediate mode community to the most hardcore of Haskell's "Arrowized FRP" GUIs. It should look familiar at first -- but different choices allow it to sidestep the complexity and the semantic hacks. The `dynamic` value turns this API into a different beast, one that has as much to teach us as it is avid on learning purely functional tricks. I'm still discovering new ideas and having fun, so I hope you will also find something cool in there. Get in touch if you do!

---

- [`game_2048.ml`](blob/master/examples/game_2048.ml) ([**demo**](https://art-w.github.io/unicorn/game_2048.html)) shows how to implement the rules of the game 2048 independently from its animations, using OCaml functors -- which also enables unit testing the rules without the extra-complexity of the rendering.
