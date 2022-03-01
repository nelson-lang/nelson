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
function tf = isunicodesupported()
    % This can be useful to decide whether to use Unicode characters or fallback ASCII characters in command-line output.
    % Note that the check is quite naive BUT it works.
    if strcmp(getenv('NELSON_TERM_IS_UNICODE_SUPPORTED'), 'TRUE')
        tf = true;
        return
    end
    if contains(getnelsonmode(), {'GUI', 'ADVANCED_SIO_CLIENT', 'BASIC_SIO_CLIENT'})
        tf = true;
        return
    end

    tf = false;
    if ~ispc()
        tf = ~strcmp(getenv('TERM'), 'linux');
        return
    end
    tf = ~isempty(getenv('WT_SESSION')) || ...
    strcmp(getenv('ConEmuTask'),  '{cmd::Cmder}') || ...
    strcmp(getenv('TERM_PROGRAM'), 'vscode') || ...
    strcmp(getenv('TERM'), 'xterm-256color') || ...
    strcmp(getenv('TERM'), 'alacritty');
end
%=============================================================================
