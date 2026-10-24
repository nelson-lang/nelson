// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
function DeploymentShortcut(Desktop: Boolean): Boolean;
begin
  Result := False;
  if PortableInstall then Exit;
  if Desktop then Result := ExpandConstant('{param:NELSONDESKTOP|0}') = '1'
  else Result := (ExpandConstant('{param:NELSONCLI|0}') <> '1') or
    (ExpandConstant('{param:NELSONSTARTMENU|0}') = '1');
end;
