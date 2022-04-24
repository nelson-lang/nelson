How to package Nelson on Chocolatey:

```
choco pack
choco install nelson.0.6.2.1745.nupkg -dv -s .

choco apikey -k <your key here> -s https://push.chocolatey.org/
choco push nelson.0.6.2.1745.nupkg -s https://push.chocolatey.org/
```
