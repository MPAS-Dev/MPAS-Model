#!/usr/bin/env python3
"""
Apply O(10^-14) perturbation to the theta (potential temperature) field
in an MPAS initial conditions file to generate ensemble diversity.

This is a placeholder until native perturbation support is added to MPAS-A.
Each ensemble member uses a unique seed for reproducible perturbations.
"""

import argparse
import sys

import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:
    print("ERROR: netCDF4 is required. Install with: pip install netCDF4")
    sys.exit(1)


def perturb_theta(ic_file, seed, magnitude=1e-14):
    rng = np.random.default_rng(seed)

    with Dataset(ic_file, "r+") as ds:
        if "theta" not in ds.variables:
            print(f"ERROR: 'theta' variable not found in {ic_file}")
            print(f"  Available variables: {list(ds.variables.keys())[:20]}...")
            sys.exit(1)

        theta = ds.variables["theta"]
        data = theta[:]
        original_mean = float(np.mean(data))

        perturbation = rng.uniform(-magnitude, magnitude, size=data.shape)
        theta[:] = data * (1.0 + perturbation)

        actual_max = np.max(np.abs(perturbation))
        print(f"Applied perturbation to theta field:")
        print(f"  File:      {ic_file}")
        print(f"  Format:    {ds.data_model}")
        print(f"  Seed:      {seed}")
        print(f"  Magnitude: +/- {magnitude:.0e}")
        print(f"  Max |eps|: {actual_max:.2e}")
        print(f"  Shape:     {data.shape}")
        print(f"  Var dtype: {theta.dtype}")
        print(f"  Original mean: {original_mean:.15e}")

    # Read-back verification: reopen and confirm perturbation persisted
    with Dataset(ic_file, "r") as ds:
        verify = ds.variables["theta"][:]
        verify_mean = float(np.mean(verify))
        diff = verify.astype(np.float64) - data.astype(np.float64)
        n_changed = int(np.count_nonzero(diff))
        max_diff = float(np.max(np.abs(diff)))
        print(f"  Verify mean:   {verify_mean:.15e}")
        print(f"  Changed cells: {n_changed}/{diff.size}")
        print(f"  Max |diff|:    {max_diff:.6e}")
        if n_changed == 0:
            print(f"ERROR: Perturbation did NOT persist in file!")
            print(f"  On-disk dtype: {ds.variables['theta'].dtype}")
            print(f"  If dtype is float32, perturbations below ~1.2e-7 will be rounded away.")
            sys.exit(1)


def main():
    parser = argparse.ArgumentParser(
        description="Perturb MPAS theta field for ensemble generation"
    )
    parser.add_argument("ic_file", help="Path to MPAS initial conditions NetCDF file")
    parser.add_argument("--seed", type=int, required=True,
                        help="Random seed for reproducible perturbation")
    parser.add_argument("--magnitude", type=float, default=1e-14,
                        help="Perturbation magnitude (default: 1e-14)")
    args = parser.parse_args()

    perturb_theta(args.ic_file, args.seed, args.magnitude)


if __name__ == "__main__":
    main()
