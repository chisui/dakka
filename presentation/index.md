---
title: Dakka
subtitle: A dependently typed actor framework for haskell
author: Philipp Dargel
date: 21.03.2019
---

# Ablauf

* Grundlagen
* Umsetzung
  * Architektur
  * Code
* Ergbenis und Ausblick

# Grundlagen

# Aktor Model

Aktoren sind eine Modellierung nebenläufiger Programme und verteilte Systeme.

# Aktor Model

Als Reaktion auf eine Nachricht kann ein Aktor

* endlich viele neue Aktoren erzeugen
* endlich viele Nachrichten an Aktoren senden
* sein Reaktionsverhalten auf nachfolgende Nachrichten verändern

# Akka

Aktor Implementierung in Scala

# cloud-haskell

* *Erlang-style* concurrent and distributed programming in Haskell.

# Umsetzung

# Haskell Language extensions

# Ziele

* Akka-ähnliche API
* Kontrollierter Umgang mit `IO`
* Typsicherheit falls möglich

# Architektur

* Aktoren werden durch einen eigenen Datentyp dargestellt.
* Aktoren implementieren `Actor` Klasse.
* `Actor` definiert Verhalten in eigener Monade.

# ActorAction

# Actor

# MockActorAction

# DistributedActorAction

* Serialisierung von Nachrichten.

# Weitere Themen

* Code Aufräumen.
* `ActorAction` durch freie `Eff` Monade ersetzten.
* type family zum Enumerieren von Aktoren. 
* Polymorphe Aktoren.
* Mehr Akka features.
* bessere Fehlernachrichten.
* Aktor definition vereinfachen.

# Fragen?

