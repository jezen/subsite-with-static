I'm trying to build a working example of a Yesod subsite which bundles its own
static files, so that these static files don't need to be passed in from the
master site.

---

To run this project locally, first install Nix and enable flakes. Then:

```sh
nix develop --impure
```

Once you're inside the shell, run GHCi:

```sh
ghci
```

Finally, in GHCi, run this command:

```
:serve
```

The local development server will be available at http://localhost:3000/.
