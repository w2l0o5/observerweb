Observer Web
============
Observer Web is Erlang observer web frontend, base code borrowed from observer gui.

## Feature
Currently supported:
* System
* Load Charts
* Memory Allocators
* Processes (preview)

## Usage
Build and relese.
```bash
make rel
```
To start the release in the foreground.
```bash
./rel/observerweb/bin/observerweb console
```
Open http://127.0.0.1:8080 in your browser


## Screenshots
![image](https://github.com/freecnpro/ObserverWeb/blob/master/screenshots/screenshot_01.png)
![image](https://github.com/freecnpro/ObserverWeb/blob/master/screenshots/screenshot_02.png)
![image](https://github.com/freecnpro/ObserverWeb/blob/master/screenshots/screenshot_03.png)
![image](https://github.com/freecnpro/ObserverWeb/blob/master/screenshots/screenshot_04.png)

## License

    Copyright 2014-2015 Freecnpro.net

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
