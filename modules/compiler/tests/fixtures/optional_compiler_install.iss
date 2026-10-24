; Copyright (c) 2016-present Allan CORNET (Nelson)
; SPDX-License-Identifier: LGPL-3.0-or-later
; No registry, shortcuts, preferences, external commands or uninstall entries.
#define APPLICATION_VERSION "0.0.0-test"
#define RootPath StageRoot + "\"
#define BinPath "x64"
[Setup]
AppId=NelsonCompilerIsolatedTest
AppName=Nelson compiler isolated test
AppVersion={#APPLICATION_VERSION}
DefaultDirName={#StageRoot}\unused-install
PrivilegesRequired=lowest
Uninstallable=no
CreateUninstallRegKey=no
UsePreviousAppDir=no
UsePreviousTasks=no
UsePreviousSetupType=no
UsePreviousLanguage=no
DisableProgramGroupPage=yes
CloseApplications=no
RestartApplications=no
AllowCancelDuringInstall=no
Compression=zip
SolidCompression=no
OutputBaseFilename=compiler-test
[Languages]
Name: english; MessagesFile: compiler:Default.isl
Name: french; MessagesFile: compiler:Languages\French.isl
[CustomMessages]
#include RepositoryRoot + "\tools\innosetup\custommessages.iss"
[Types]
#include RepositoryRoot + "\tools\innosetup\types.iss"
[Components]
#include RepositoryRoot + "\tools\innosetup\components.iss"
[Files]
#include RepositoryRoot + "\modules\compiler\module.iss"
#include RepositoryRoot + "\modules\modules_manager\module.iss"
[Code]
#ifdef PRODUCTION_CODE
// This variant is compiled only, never executed.
#include RepositoryRoot + "\tools\innosetup\code.iss"
#else
#include RepositoryRoot + "\tools\innosetup\compiler_dependencies.iss"
#endif
