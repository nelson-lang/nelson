
# Setup Julia environment (CI)
setup_julia:
    ./bin/{{os}}/nelson-cli --no-ipc --quiet -f $GITHUB_WORKSPACE/tools/julia_environment_CI/configureJuliaEnvironment.m
