---
title: "Dependently typed Actor framework in Haskell" 
author:
- Philipp Dargel
date: 2018-??-??
tags:
- Bachelor thesis
- Haskell
- Actor
- Dependent types
titlepage: yes
toc-own-page: yes
---

# Introduction

The goal of this thesis is to create an Actor framework, simlar to akka for Haskell that leverages 

```haskell
main :: IO ()
main = putStrLn "some Haskell Code"
```

```{include=src/Dakka/Actor.hs}
```

