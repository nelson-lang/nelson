%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
bdclose('all');
new_system('m');
add_block('nflow/math/gain', 'm/A');
add_block('nflow/math/gain', 'm/B');
add_line('m', 'A/1', 'B/1');
%=============================================================================
% deleting an existing connection succeeds
delete_line('m', 'A/1', 'B/1');
%=============================================================================
% the connection is gone, so deleting it again raises
asserts.checkerror('delete_line(''m'', ''A/1'', ''B/1'')', _('No such connection to delete.'));
%=============================================================================
% the input port is free again: a new line can be drawn and removed
add_line('m', 'A/1', 'B/1');
delete_line('m', 'A/1', 'B/1');
%=============================================================================
% an unknown model is reported
asserts.checkerror('delete_line(''nope'', ''A/1'', ''B/1'')', _('Model not loaded: nope'));
%=============================================================================
% the three ports are mandatory
asserts.checkerror('delete_line(''m'')', _('Not enough input arguments.'));
%=============================================================================
bdclose('all');
%=============================================================================
