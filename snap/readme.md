# Build

```
snapcraft clean
snapcraft
```

# Test locally

```
sudo snap install nelson_0.6.4_amd64.snap --dangerous
```

# Push

```
snapcraft upload nelson_0.6.4_amd64.snap
```

# Connection

```
sudo snap connect nelson:jack
sudo snap connect nelson:home
sudo snap connect nelson:removable-media
```
