module rpg {
    type Option[a] =
    | Some(a)
    | None

    type TipoCriatura = Sacerdote | Bardo | Necromante | Inimigo

    type TipoEfeito = Cura | Canto | Lanca

    type Efeito = {
        tipo: TipoEfeito,
        hp: int
    }

    type Criatura = {
        hp: int,
        classe: TipoCriatura,
        paralisia: bool,
        efeito: Option[Efeito],
        iniciativa: int
    }

    var criaturas: List[Criatura]
    var criatura: int
    var turno: int
    var info_turno: Option[{ jogador: Criatura, vitima: Option[Criatura], acao_tomada: str, dano: Option[int] }]

    pure def paralisar(c: Criatura, p: bool): Criatura = { ...c, paralisia: p }

    pure def ordena_criaturas(criaturas: List[Criatura]): List[Criatura] =
        criaturas.foldl([], (acc, criatura) => 
            val antes = acc.select(c => criatura.iniciativa < c.iniciativa)
            val depois = acc.select(c => criatura.iniciativa >= c.iniciativa)
            antes.append(criatura).concat(depois)
        )

    action paralisar_personagem(inimigo: Criatura, pos_personagem: int): bool = {
        val personagem = criaturas[pos_personagem]
        all {
            personagem.hp > 0,
            not(personagem.paralisia),
            inimigo.classe == Inimigo,
            personagem.classe != Inimigo,
            criaturas' = criaturas.replaceAt(pos_personagem, personagem.paralisar(true)).replaceAt(criatura, { ...inimigo, efeito: None }),
            info_turno' = Some({ jogador: inimigo, vitima: Some(personagem), acao_tomada: "paralisar_personagem", dano: None })
        }
    }

    action remover_paralisia(personagem: Criatura, pos_personagem_paralisado: int): bool = {
        val personagem_paralisado = criaturas[pos_personagem_paralisado]
        all {
            personagem_paralisado.hp > 0,
            personagem_paralisado.paralisia,
            personagem_paralisado.classe != Inimigo,
            personagem.classe != Inimigo,
            personagem != personagem_paralisado,
            criaturas' = criaturas.replaceAt(pos_personagem_paralisado, personagem_paralisado.paralisar(false)).replaceAt(criatura, { ...personagem, efeito: None }),
            info_turno' = Some({ jogador: personagem, vitima: Some(personagem_paralisado), acao_tomada: "remover_paralisia", dano: None })
        }
    }

    action cura_sacerdote(personagem: Criatura): bool = {
        all {
            personagem.hp > 0,
            personagem.classe == Sacerdote,
            not(personagem.paralisia),
            criaturas' = aumentar_hp(criaturas),
            info_turno' = Some({ jogador: personagem, vitima: None, acao_tomada: "Cura sacerdote", dano: None })
        }
    }

     pure def aumentar_hp(criaturas: List[Criatura]): List[Criatura] =
        criaturas.foldl([], (acc, criatura) =>
            if (criatura.classe != Inimigo and criatura.hp > 0){
                val hp_ajustado = if(criatura.hp > 10) 20 else criatura.hp + 10
                acc.append(criatura)
            }
            else {
                acc.append(criatura)
            }
        )

    action canto_bardo(personagem: Criatura): bool = {
        all {
            personagem.hp > 0,
            personagem.classe == Bardo,
            not(personagem.paralisia),
            criaturas' = criaturas.replaceAt(criatura, { ...personagem, efeito: None }),
            info_turno' = Some({ jogador: personagem, vitima: None, acao_tomada: "Canto bardo", dano: None })
        }
    }

    action ataque(agressor: Criatura): bool = {
        // // Escolhendo a vitima com filtro para ajudar o simulador, antes recebia a possivel_vitima escolhida no step mas o monstro nunca morria
        // nondet pos_vitima = criaturas.indices().filter(x => x != criatura and if (agressor.classe == Monstro) criaturas[x].classe != Monstro else criaturas[x].classe == Monstro).oneOf()

        // val vitima = criaturas[pos_vitima]
        // val d = match agressor.efeito {
        //     | Some(e) => if (agressor.classe == Monstro) 0 else 10
        //     | None => if (agressor.classe == Monstro and turno != 1) 20 else 10
        // }
        // all {
        //     vitima.hp > 0,
        //     (agressor.classe == Inimigo and criaturas.select(x => x.ovelha_ou_urso()).length() > 0)
        //         implies 
        //     vitima.ovelha_ou_urso(),
        //     criaturas' = criaturas.replaceAt(pos_vitima, vitima.dano(d)).replaceAt(criatura, { ...agressor, efeito: None }),
        //     info_turno' = Some({ jogador: agressor, vitima: Some(vitima), acao_tomada: "ataque", dano: Some(d) })
        // }
    }

    
    action init = {
        val dado = 1.to(20)
        nondet ini_sacerdote = dado.oneOf()
        nondet ini_bardo = dado.oneOf()
        nondet ini_necromante = dado.oneOf()
        nondet ini_inimigo = dado.oneOf()

        all{
                criaturas' = ordena_criaturas(
                [
                    { hp: 20, classe: Sacerdote, paralisia: false, efeito: None, iniciativa: ini_sacerdote },
                    { hp: 20, classe: Bardo, paralisia: false, efeito: None, iniciativa: ini_bardo},
                    { hp: 20, classe: Necromante, paralisia: false, efeito: None, iniciativa: ini_necromante},
                    { hp: 90, classe: Inimigo, paralisia: false, efeito: None, iniciativa: ini_inimigo}
                ]),

            criatura' = 0,
            turno' = 1,
            info_turno' = None
        }
    }

    action step = {
        val jogador = criaturas[criatura]
        nondet possivel_vitima = criaturas.indices().oneOf()

        all {
            jogador.hp > 0,
            not(jogador.paralisia),
            any {
                paralisar_personagem(jogador, possivel_vitima),
                remover_paralisia(jogador, possivel_vitima),
                jogador.cura_sacerdote(),
                jogador.canto_bardo(),
            },
            criatura' = if (criatura + 1 == criaturas.length()) 0 else criatura + 1,
            turno' = if (criatura + 1 == criaturas.length()) turno + 1 else turno,
        }
    }

    val monstro_nao_morre = criaturas.indices().filter(i => criaturas[i].classe == Inimigo).forall(i => criaturas[i].hp > 0)
    val personagens_nao_morrem = criaturas.indices().filter(i => criaturas[i].classe != Inimigo).forall(x => criaturas[x].hp > 0)

}