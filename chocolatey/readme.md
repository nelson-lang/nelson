### How to package Nelson on Chocolatey:

## Get api key here:

```
https://community.chocolatey.org/account/Packages
```

```
choco pack
choco install nelson.v0.7.10.3376.nupkg -dv -s .

choco apikey -k <your key here> -s https://push.chocolatey.org/
choco push nelson.0.7.10.3376.nupkg -s https://push.chocolatey.org/
```
