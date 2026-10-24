%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[zones, version] = timezones();
asserts.istrue(isstring(zones));
asserts.istrue(any(strcmp(cellstr(zones), 'Europe/Paris')));
asserts.istrue(any(strcmp(cellstr(zones), 'America/New_York')));
asserts.isequal(version, "nelson-minimal-2026");
%=============================================================================
