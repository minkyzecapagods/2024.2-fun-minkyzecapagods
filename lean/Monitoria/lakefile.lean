import Lake
open Lake DSL

package «FMCn_Lean» where
  -- add package configuration options here

lean_lib «FMCnLean» where
  -- add library configuration options here

@[default_target]
lean_exe «fmcn_lean» where
  root := `Main
