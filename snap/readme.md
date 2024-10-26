# Build

```bash
NELSON_VERSION=$(grep -oP '"version": "\K[^"]+' package.json)
echo $NELSON_VERSION
```

```bash
snapcraft clean
snapcraft
```

Alternative:

```bash
sudo snapcraft clean
sudo rm -rf stage
sudo rm -rf parts
sudo rm -rf prime
sudo snapcraft --destructive-mode --verbose
```

# Test locally

```bash
sudo snap install nelson_${NELSON_VERSION}_amd64.snap --dangerous
```

Debug:

```bash
snap run --shell nelson.nelson
```

# Push

```bash
snapcraft upload nelson_${NELSON_VERSION}_amd64.snap
```

# Connection

```bash
sudo snap connect nelson:jack
sudo snap connect nelson:home
sudo snap connect nelson:removable-media
```
