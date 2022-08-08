# JPLib

![](https://tokei.rs/b1/github/jackdp/JPLib?category=code)
![](https://tokei.rs/b1/github/jackdp/JPLib?category=files)
![visitors](https://visitor-badge.glitch.me/badge?page_id=jackdp.jplib)
![GitHub Repo size](https://img.shields.io/github/repo-size/jackdp/JPLib)

A library of general-purpose pascal units needed to compile my projects published on GitHub.

Supported Delphi versions: **2009**, **2010**, **XE**, **XE2**, **XE3**, **XE4**, **XE5**, **XE6**, **XE7**, **XE8**, **10.0 Seattle**, **10.1 Berlin**, **10.2 Tokyo**, **10.3 Rio**, **10.4 Sydney**, **11.0 Alexandria**.  
**FPC** (Free Pascal Compiler): required version ~~3.0.4~~ **3.2.0** or newer.

## Installation

Installation packages for all supported IDEs can be found in the [packages](./packages) folder.
The packages are "run-time" type and do not contain any components.

The JPLib package must be installed if you want to install the [JPPack](https://github.com/jackdp/JPPack) components package. If you do not want to install JPPack, you can use JPLib without any installation.

## Hash

Some units in the [Hash](./Hash) folder uses **HashLib4Pascal** library from <https://github.com/Xor-el/HashLib4Pascal> and **Wolfgang Ehrhardt's** hashing units from <https://github.com/jackdp/www.wolfgang-ehrhardt.de/tree/master/src/crc-hash/crc_hash>

## License

The license for my work: **public domain**. You can do with my code whatever you want without any limitations.

But in some units I use code from other open source projects, so you should look at the PAS source files and license of the authors of these projects for more information.

## Important changes

<mark>A list of changes that may break existing code.</mark>

2022.08.06  
JPL.Conversion.pas  
```pascal
function TryGetMilliseconds(const NumStr: string; out MilliSeconds: Int64;
  DefaultTimeUnit: TTimeUnit = tuMillisecond): Boolean;
```
The `DefaultTimeUnit` parameter has been added. It specifies the default time unit to be used if it is not given in `NumStr`. Now the default unit is a **millisecond**, before the change it was a second.
