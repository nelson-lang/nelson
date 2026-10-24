%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(max([1 NaN 3],[],'omitmissing'), 3);
asserts.isequal(max([1 NaN 3],[],'includemissing'), NaN);
asserts.isequal(max(zeros(0,3),[],'all'), zeros([0 1], 'double'));
asserts.isequal(max(1,NaN,'omitmissing'), 1);
asserts.isequal(max([1 2],[],2,'linear'), 2);
asserts.isequal(max([1 NaN;3 4],[],[1 2],'omitnan'), 4);
asserts.checkerror('max(1,[],3,4)', _('Invalid 4th argument.'));
asserts.isequal(max(1i,1), complex(0, 1));
asserts.isequal(max(-1,1i), -1);
asserts.isequal(max(1+1i,1-1i), complex(1, 1));
asserts.checkerror('max(ones(2,3,4),[],[2 2])', _('Elements of a dimension vector must be unique.'));
%=============================================================================
