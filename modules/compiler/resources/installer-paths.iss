// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later

function RuntimeAttributes(Name: String): Cardinal;
  external 'GetFileAttributesW@kernel32.dll stdcall';

procedure RequireRuntime(Condition: Boolean; Message: String);
begin
  if not Condition then RaiseException(Message);
end;

procedure CheckNoRedirect(Name: String);
var Parent: String; Attributes: Cardinal; First: Boolean;
begin
  First := True;
  while Name <> '' do begin
    Attributes := RuntimeAttributes(InstallNativePath(Name));
    if Attributes <> $FFFFFFFF then begin
      RequireRuntime((Attributes and $400) = 0, 'Runtime paths cannot contain reparse points.');
      RequireRuntime(First or ((Attributes and $10) <> 0), 'Runtime ancestor is not a directory.');
    end;
    First := False;
    Parent := ExtractFileDir(Name);
    if Parent = Name then Break;
    Name := Parent;
  end;
end;
