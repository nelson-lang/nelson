%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% A pure missing array used to lose its data over ipc (the payload was never
% written by the serializer). Check the class and the missing state survive.
clear('R');
REF = missing();
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'missing');
asserts.istrue(ismissing(R));
%=============================================================================
clear('R');
REF = repmat(missing(), 2, 3);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'missing');
asserts.isequal(size(R), [2, 3]);
asserts.istrue(all(ismissing(R(:))));
%=============================================================================
clear('R');
REF = [1, missing(), 3; missing(), 5, missing()];
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(size(R), [2, 3]);
asserts.isequal(ismissing(R), ismissing(REF));
asserts.istrue(isequaln(double(R), double(REF)));
%=============================================================================
