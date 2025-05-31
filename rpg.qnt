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
            },
            criatura' = if (criatura + 1 == criaturas.length()) 0 else criatura + 1,
            turno' = if (criatura + 1 == criaturas.length()) turno + 1 else turno,
        }
    }


}