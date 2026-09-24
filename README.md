# 📈 Rates Scope

Interest rates derivatives pricing library.

**🚧 Work in Progress**

## JSON-RPC API

The `json-rpc/` module exposes four main methods:

- **`price`** - Price caplets, swaptions, and backward-looking caplets
- **`arbitrage`** - Check volatility arbitrage for specific tenor/expiry
- **`arbitrage-matrix`** - Full arbitrage matrix across all tenors/expiries
- **`vol-sampling`** - Volatility smile sampling with PDF calculations

## Usage 🔍

```bash
# Build project
sbt compile
# Run JSON-RPC server
sbt "json-rpc/run"
# Run tests
sbt test
```
