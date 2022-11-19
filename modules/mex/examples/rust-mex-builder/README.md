# Example to create/build a MEX function with Nelson using Rust

## Building

Rust must be in PATH environment variable.

On windows x64:

```matlab
example_path = [nelsonroot(), '/modules/mex/examples/rust-mex-builder/helloworld/'];
cd(example_path)
txt = fileread('build.template');
txt = replace(txt, '__LIB_PATH__', modulepath(nelsonroot(),'mex','bin'));
filewrite('build.rs', txt);
[s, msg] = system('cargo build');
copyfile([example_path, '/target/debug/helloworldmex', getdynlibext()], [example_path, '/target/debug/helloworldmex.', mexext()]);
addpath('.\target\debug\');
helloworldmex
```
