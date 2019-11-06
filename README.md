# Purescript-parsing example

This is a small example that demonstrates how to parse a very simple DSL using the [purescript-parsing library](https://pursuit.purescript.org/packages/purescript-parsing/5.0.3).

If you don't want to install the [purescript](http://www.purescript.org/) tooling and the [package manager spago](https://github.com/spacchetti/spago) locally, you can instead use install [Docker](https://www.docker.com/get-started), [VS Code](https://code.visualstudio.com/) and the [VS Code Remote Containers extension](https://code.visualstudio.com/docs/remote/containers#_using-an-image-or-dockerfile) (check the system requirements of the remote containers extension, not all Docker/OS versions work for this). Once this is installed if you open this repo you should get a message asking you if you want to run this inside a dev container. If you say yes it should set the entire language tooling up inside the container and you can develop it this way. In the beginning you might have to run `spago build` to do the initial build to get the editor tooling working. If after a package install the tooling doesn't recogize a library, restart the purescript ide tooling inside VS Code with CTLR+P -> "restarte purescript IDE server".

Run the project from a terminal with
```bash
spago run
```
