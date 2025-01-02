# =============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
# =============================================================================
# This file is part of the Nelson.
# =============================================================================
# LICENCE_BLOCK_BEGIN
# SPDX-License-Identifier: LGPL-3.0-or-later
# LICENCE_BLOCK_END
# =============================================================================
display("Hello from Julia")

A = [1, 2, 3]
B = [4, 5, 6]

C = A .+ B .+ D  # Use element-wise addition
display(C)
