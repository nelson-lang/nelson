%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
destinationdir = [tempdir(), 'mex_engine_demo_1/'];
if isdir(destinationdir)
    rmdir(destinationdir, 's');
end
mkdir(destinationdir);
destination = [destinationdir, 'mex_engine_demo_1.c'];
status = copyfile('mex_engine_demo_1.c', destinationdir);
cd(destinationdir);
mex('-client', 'engine', 'mex_engine_demo_1.c');
generated_executable = [destinationdir, 'mex_engine_demo_1'];
disp('Open an Terminal')
disp('Set environment variables to find Nelson (see doc mex).')
disp('Go to:')
disp(destinationdir)
disp('Launch')
disp('mex_engine_demo_1')
%=============================================================================
