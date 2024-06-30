# Build

```
snapcraft clean
snapcraft
```

Alternative:

```
sudo snapcraft clean
sudo snapcraft --destructive-mode  --verbose
```

# Test locally

```
sudo snap install nelson_1.6.0.0_amd64.snap --dangerous
```

Debug:

```
snap run --shell nelson.nelson
```

# Push

```
snapcraft upload nelson_1.6.0.0_amd64.snap
```

# Connection

```
sudo snap connect nelson:jack
sudo snap connect nelson:home
sudo snap connect nelson:removable-media
```
