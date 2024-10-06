# Build

```bash
NELSON_VERSION=$(grep -oP '"version": "\K[^"]+' package.json)

```

```bash
snapcraft clean
snapcraft
```

Alternative:

```bash
sudo snapcraft clean
sudo snapcraft --destructive-mode --verbose
```

# Test locally

```bash
sudo snap install nelson_$NELSON_VERSION_amd64.snap --dangerous
```

Debug:

```bash
snap run --shell nelson.nelson
```

# Push

```bash
snapcraft upload nelson_$NELSON_VERSION_amd64.snap
```

# Connection

```bash
sudo snap connect nelson:jack
sudo snap connect nelson:home
sudo snap connect nelson:removable-media
```
