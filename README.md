Observer Web
============
**This project will no longer be maintained.** 
Observer Web is erlang observer web frontend, base code borrowed from observer gui.

## Feature
Currently supported:
* System
* Load Charts
* Memory Allocators
* Processes (preview)

## TODO

- [ ] Applications
- [x] Processes
- [ ] Table viewer
- [ ] Trace Overview 

## Usage
Build and relese.
```bash
make rel
```
To start the release in the foreground.
```bash
./_build/default/rel/observerweb/bin/observerweb console
```
Open http://127.0.0.1:8080 in your browser


## Screenshots
![image](https://github.com/freecnpro/ObserverWeb/blob/master/screenshots/screenshot_01.png)
![image](https://github.com/freecnpro/ObserverWeb/blob/master/screenshots/screenshot_02.png)
![image](https://github.com/freecnpro/ObserverWeb/blob/master/screenshots/screenshot_03.png)
![image](https://github.com/freecnpro/ObserverWeb/blob/master/screenshots/screenshot_04.png)

## License

    The MIT License (MIT)
