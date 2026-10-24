%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
qualityInfo = tsdata.qualmetadata('Code', [0 1], 'Description', {'ok', 'bad'});
asserts.isequal(qualityInfo.Code, [0 1]);
asserts.isequal(qualityInfo.Description, {'ok'; 'bad'});
qualityInfo = set(qualityInfo, 'UserData', 42);
asserts.isequal(qualityInfo.UserData, 42);
%=============================================================================
