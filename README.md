##About

A simple ObjViewer. Might be useful as Parsec example and for everyone trying to figure out how to use OpenGL with Haskell.

I've written this quite some time ago and recently wanted to refresh my haskell knowledge a bit, so I fixed this up and tried understanding the code I produced back then.

Turns out the decisions I made back then were less then optimal, especially I should not have used Parsec for parsing the Wavefront Obj format. It is overkill and quite slow for the task.

There were also numerous problems with the parsing, which should be fixed, but I can't guarantee that this works for every .obj you throw at it. Also the code is becoming messier the more I work around issues, so I'll just leave it as it is.

When I wrote this viewer I later took the code and made my GameState engine out of it, this explains why I have stuff like Primitives.hs and Texture.hs in there, without really making use of them.

##Usage

If you have a working [haskell stack](https://docs.haskellstack.org/en/stable/README/) setup:

    git clone https://github.com/rakete/ObjViewer
    cd ObjViewer
    stack build

Then use:

    stack exec ObjViewer-exe <objfile>

to run. Try it on the .obj files in ./test/, those should work.

You can zoom in and out using the mouse wheel. If you press the middle mouse button once you will turn arcball mode, where you can rotate the camera around the object, pressing middle mouse again will turn it off. Pressing w will toggle the wireframe display.

###What works

- Loading and displaying meshes, even very large ones.
- Rotating the mesh, zooming in and out.
- Display wireframe of mesh.
- Material definitions are parsed and used to set parameters of OpenGL fixed function pipeline.
- Transparent faces mixed with opaque ones render in correct order.
- Smooth groups, there can be both smooth and hard edges in a mesh, and they are shaded correctly.

###What does not work

- There is no support for textures.
