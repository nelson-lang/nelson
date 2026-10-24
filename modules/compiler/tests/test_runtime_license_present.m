%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% compiler.package.installer builds its runtime from the files found at
% nelsonroot(). The runtime (and the standalone installer audit) expects a
% LICENSE notice next to the other redistribution notices, so an installed
% Nelson must ship it, exactly like gpl-3.0.md and lgpl-3.0.md.
asserts.istrue(isfile([nelsonroot(), '/LICENSE']));
asserts.istrue(isfile([nelsonroot(), '/gpl-3.0.md']));
asserts.istrue(isfile([nelsonroot(), '/lgpl-3.0.md']));
asserts.istrue(isfile([nelsonroot(), '/THIRDPARTY.md']));
asserts.istrue(isfile([nelsonroot(), '/TRADEMARKS.md']));
%=============================================================================
% Guard the installer audit on the source tree: root.iss is not shipped.
rootIss = [nelsonroot(), '/root.iss'];
if isfile(rootIss)
  installer = fileread(rootIss);
  asserts.istrue(contains(installer, 'Source: {#RootPath}LICENSE; DestDir: {app}\'));
end
%=============================================================================
