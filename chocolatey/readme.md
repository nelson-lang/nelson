### How to package Nelson on Chocolatey:

## Get api key here:

```
https://community.chocolatey.org/account/Packages
```

```
choco pack
choco install nelson.1.15.0.5475.nupkg -dv -s .

choco apikey -k <your key here> -s https://push.chocolatey.org/
choco push nelson.1.15.0.5475.nupkg -s https://push.chocolatey.org/
```
