# Security Policy

Nelson is free and open-source software distributed under the GNU Lesser
General Public License v3. This policy describes how security issues are
reported, triaged, and disclosed. It also documents the practices that let
downstream integrators use Nelson with confidence, including those subject to
the EU Cyber Resilience Act (Regulation (EU) 2024/2847).

## Supported Versions

Security fixes are provided for the latest released minor line. Older versions
receive fixes only at the maintainers' discretion.

| Version | Supported          |
| ------- | ------------------ |
| 2.0.x   | :white_check_mark: |
| < 2.0   | :x:                |

Users are strongly encouraged to run the latest release. Fixes are generally
delivered in a new patch or minor release rather than as standalone backports.

## Reporting a Vulnerability

Please report vulnerabilities **privately**. Do not open a public issue, pull
request, or discussion for a suspected vulnerability.

Preferred channels:

- **GitHub private advisory:** use *Security > Report a vulnerability* on the
  repository (GitHub Security Advisories), or
- **Email:** nelson.numerical.computation@gmail.com

To help triage, please include where possible:

- affected version(s), operating system, and build configuration;
- a description of the issue and its impact;
- steps to reproduce, a proof of concept, or a minimal `.m` script;
- any known mitigation or workaround;
- how you would like to be credited.

If you wish to send encrypted details, mention it in your first message and a
key or secure channel will be provided.

## Coordinated Disclosure and Response Targets

We follow a coordinated (responsible) disclosure process. Nelson is maintained
by volunteers, so we do not commit to fixed response deadlines. As a best
effort we will:

- acknowledge your report as soon as we reasonably can;
- assess and triage the issue, and keep you informed of progress;
- work on a fix or mitigation, prioritized by severity.

By default a vulnerability is disclosed publicly only once a fix or mitigation
is available, and coordinated with the reporter. We aim to publish an advisory
and credit the reporter (unless anonymity is requested). Reporters are asked to
give us a reasonable amount of time to remediate before any public disclosure.

## Scope

In scope:

- the Nelson interpreter, built-in functions, and modules in this repository;
- the official binary packages published by the project;
- the package/module tooling distributed with Nelson.

Out of scope:

- vulnerabilities solely in third-party dependencies (please also report them
  upstream; we will update the affected dependency);
- issues affecting only unsupported versions;
- third-party modules and packages not maintained in this repository;
- results that require a pre-compromised host, a malicious local user with
  existing privileges, or physical access;
- crashes triggered only by intentionally malformed input where no memory
  safety or privilege boundary is crossed (please still report these as regular
  bugs).

Nelson executes user-provided scripts by design. Running an untrusted `.m`
script, like running any untrusted program, is not in itself considered a
vulnerability.

## Supply-Chain and Dependencies

Nelson bundles several third-party components (vendored libraries). We track
these components and update them when security fixes are published upstream. If
you need a Software Bill of Materials (SBOM) for a compliance review, please
contact the maintainers.

When verifying downloaded releases, use the checksums and any signatures
published alongside the official release artifacts, and obtain binaries only
from the official distribution channels.

## For Downstream and Commercial Integrators

This project is developed and published as open-source software. Organizations
that integrate Nelson into a commercial product with digital elements are
responsible for the security and regulatory conformity (including CRA
obligations) of that product as a whole. The maintainers will, on a best-effort
basis, cooperate on vulnerability handling and provide the technical
information needed for such reviews.

## Credits

We are grateful to the researchers and users who report issues responsibly.
Unless you ask to remain anonymous, your contribution will be acknowledged in
the corresponding advisory or release notes.
