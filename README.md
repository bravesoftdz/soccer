# soccer

soccer is an engine for collective decision making. In the current version you can organize voting elections to pick a winning alternative using chosoc (choice language)

## Getting Started

1. Download archive from [Releases](https://github.com/Zawuza/soccer/releases), unzip it into some folder and add this folder to PATH variable

2. Copy-paste the following into a getstarted.chosoc file

```
START[voting]       // start an election \\
IMPORT[plurality]   // import a rule that describes how to pick a solution \\
VOTE(pizza->fruits) // vote that pizza is better than fruits \\
VOTE(fruits->pizza) // add more voters \\
VOTE(fruits->pizza) 
DECIDE!
```
3. Run in command line:

```
soccer .\getstarted.chosoc file
```
4. Look at the winner
```
Selected with plurality
Winners: fruits
```

## Documentation

See our [wiki](https://github.com/Zawuza/soccer/wiki) for an extensive documentation of soccer and chosoc.
You can also explore samples delivered with our program.

## Running the tests

We have different tests for different parts of program written in Delphi using DUnitX

## Contributing

CONTRIBUTING.md isn't set up, create a pull request/issue and let's see what you have :)

## Authors

* **Andrei Aleksandrov** - Main maintainer - [zawuza](https://github.com/Zawuza)

See also the list of [contributors](https://github.com/Zawuza/soccer/wiki/Contributors-and-acknowledgements) who participated in this project.

## License

This project is licensed under the Apache License 2.0

